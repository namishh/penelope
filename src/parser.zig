const std = @import("std");
const testing = std.testing;

pub const Error = error{ CouldNotMatch, ParserDidNotConsumeInput };

pub const RunResult = union(enum) {
    success: ParseResult,
    err: ParseError,
};

pub const ParseError = struct {
    err: ?Error = null,
    index: usize,
    expected: []const u8,
    found: []const u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        if (self.found.len > 0) {
            try writer.print("parse error at byte {d}: expected {s}, found '{s}'\n", .{ self.index, self.expected, self.found });
        } else {
            try writer.print("parse error at byte {d}: expected {s}, found end of input\n", .{ self.index, self.expected });
        }
    }
};

const ParserState = struct {
    input: []const u8,
    index: usize = 0,
    error_info: ?ParseError = null,

    pub fn init(input: []const u8) ParserState {
        return .{ .input = input };
    }

    pub fn remaining(self: *ParserState) []const u8 {
        return self.input[self.index..];
    }

    pub fn record_expected(self: *ParserState, expected: []const u8) void {
        const err = ParseError{
            .index = self.index,
            .expected = expected,
            .found = self.input[self.index..],
        };

        if (self.error_info == null or err.index > self.error_info.?.index) {
            self.error_info = err;
        }
    }
};

pub const Span = struct {
    start: usize,
    end: usize,

    pub fn slice(self: Span, input: []const u8) []const u8 {
        return input[self.start..self.end];
    }
};

pub const ParseResult = struct {
    span: Span,
    input: []const u8,
    consumed: usize,

    pub fn value(self: ParseResult) []const u8 {
        return self.span.slice(self.input);
    }
};

pub const Parser = union(enum) {
    string: []const u8,
    sequence: []const Parser,
    choice: []const Parser,
    lazy: *const fn () Parser,
    many: *const Parser,
    many1: *const Parser,
    not: *const Parser,
    lettersN: usize,
    digitsN: usize,
    digits,
    identifier,
    any,
    letters,
    eof,

    pub fn parse(self: Parser, state: *ParserState) Error!Span {
        const start = state.index;

        switch (self) {
            .string => |expected| {
                const remaining = state.remaining();

                if (remaining.len < expected.len or !std.mem.eql(u8, remaining[0..expected.len], expected)) {
                    state.record_expected(expected);
                    return error.CouldNotMatch;
                }
                state.index += expected.len;
            },

            .letters => {
                while (state.index < state.input.len and std.ascii.isAlphabetic(state.input[state.index])) {
                    state.index += 1;
                }

                if (state.index == start) {
                    state.record_expected("letter");
                    return error.CouldNotMatch;
                }
            },

            .lettersN => |size| {
                if (size > state.input.len - start) {
                    state.record_expected(std.fmt.allocPrint(std.heap.page_allocator, "{d} letters", .{size}) catch "letters");
                    return error.CouldNotMatch;
                }

                const end = start + size;
                while (state.index < end) {
                    if (!std.ascii.isAlphabetic(state.input[state.index])) {
                        state.record_expected("letter");
                        state.index = start;
                        return error.CouldNotMatch;
                    }
                    state.index += 1;
                }
            },

            .digitsN => |size| {
                if (size > state.input.len - start) {
                    state.record_expected(std.fmt.allocPrint(std.heap.page_allocator, "{d} digits", .{size}) catch "digits");
                    return error.CouldNotMatch;
                }

                const end = start + size;
                while (state.index < end) {
                    if (!std.ascii.isDigit(state.input[state.index])) {
                        state.record_expected("digit");
                        state.index = start;
                        return error.CouldNotMatch;
                    }
                    state.index += 1;
                }
            },

            .eof => {
                if (!(state.index >= state.input.len)) {
                    state.record_expected("end of input");
                    return error.CouldNotMatch;
                }
            },

            .any => {
                if (state.index >= state.input.len) {
                    state.record_expected("any character");
                    return error.CouldNotMatch;
                }

                state.index += 1;
            },

            .identifier => {
                if (state.index >= state.input.len) {
                    state.record_expected("identifier");
                    return error.CouldNotMatch;
                }

                const first = state.input[state.index];

                if (!(std.ascii.isAlphabetic(first) or first == '_')) {
                    state.record_expected("identifier");
                    return error.CouldNotMatch;
                }

                state.index += 1;

                while (state.index < state.input.len) {
                    const c = state.input[state.index];

                    if (std.ascii.isAlphabetic(c) or
                        std.ascii.isDigit(c) or
                        c == '_')
                    {
                        state.index += 1;
                    } else {
                        break;
                    }
                }
            },

            .digits => {
                while (state.index < state.input.len and std.ascii.isDigit(state.input[state.index])) {
                    state.index += 1;
                }

                if (state.index == start) {
                    state.record_expected("digit");
                    return error.CouldNotMatch;
                }
            },

            .sequence => |parsers| {
                const checkpoint = state.index;
                for (parsers) |parser| {
                    _ = parser.parse(state) catch |err| {
                        state.index = checkpoint;
                        return err;
                    };
                }
            },

            .many => |parser| {
                while (true) {
                    const checkpoint = state.index;
                    _ = parser.parse(state) catch |err| {
                        switch (err) {
                            error.CouldNotMatch => {
                                state.index = checkpoint;
                                break;
                            },

                            else => return err,
                        }
                    };

                    if (state.index == checkpoint) {
                        return error.ParserDidNotConsumeInput;
                    }
                }
            },

            .many1 => |parser| {
                _ = try parser.parse(state);
                while (true) {
                    const checkpoint = state.index;
                    _ = parser.parse(state) catch |err| {
                        switch (err) {
                            error.CouldNotMatch => {
                                state.index = checkpoint;
                                break;
                            },

                            else => return err,
                        }
                    };

                    if (state.index == checkpoint) {
                        return error.ParserDidNotConsumeInput;
                    }
                }
            },

            .choice => |parsers| {
                const checkpoint = state.index;
                for (parsers) |parser| {
                    state.index = checkpoint;

                    if (parser.parse(state)) |_| {
                        return .{ .start = start, .end = state.index };
                    } else |err| {
                        switch (err) {
                            error.CouldNotMatch => continue,
                            else => return err,
                        }
                    }
                }
                state.index = checkpoint;
                return error.CouldNotMatch;
            },

            .not => |parser| {
                const checkpoint = state.index;
                if (parser.parse(state)) |_| {
                    state.index = checkpoint;
                    return error.CouldNotMatch;
                } else |_| {
                    state.index = checkpoint;
                    return .{ .start = checkpoint, .end = checkpoint };
                }
            },
            .lazy => |f| {
                const parser = f();
                return parser.parse(state);
            },
        }

        return .{ .start = start, .end = state.index };
    }

    pub fn run(self: Parser, input: []const u8) RunResult {
        var state = ParserState.init(input);

        const span = self.parse(&state) catch |err| {
            const details = state.error_info orelse ParseError{ .index = state.index, .expected = "unknown", .found = "" };
            var d = details;
            d.err = err;
            return .{ .err = d };
        };

        return .{ .success = .{
            .span = span,
            .input = input,
            .consumed = state.index,
        } };
    }
};

pub fn str(value: []const u8) Parser {
    return .{ .string = value };
}

pub fn sequence(parsers: []const Parser) Parser {
    return .{ .sequence = parsers };
}

pub fn choice(parsers: []const Parser) Parser {
    return .{ .choice = parsers };
}

pub fn digits() Parser {
    return .digits;
}

pub fn letters() Parser {
    return .letters;
}

pub fn eof() Parser {
    return .eof;
}

pub fn digitsN(n: usize) Parser {
    return .{ .digitsN = n };
}

pub fn not(n: *const Parser) Parser {
    return .{ .not = n };
}

pub fn any() Parser {
    return .any;
}

pub fn identifier() Parser {
    return .identifier;
}

pub fn lettersN(n: usize) Parser {
    return .{ .lettersN = n };
}

pub fn many(p: *const Parser) Parser {
    return .{ .many = p };
}

pub fn many1(p: *const Parser) Parser {
    return .{ .many1 = p };
}

pub fn lazy(f: *const fn () Parser) Parser {
    return .{ .lazy = f };
}

test "string parser" {
    const parser = str("hello");

    switch (parser.run("hello")) {
        .success => |result| {
            try testing.expectEqualStrings("hello", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "string parser failure" {
    const parser = str("hello");
    switch (parser.run("helo")) {
        .success => |_| try testing.expect(false),
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expectEqualStrings("hello", e.expected);
            try testing.expectEqualStrings("helo", e.found);
        },
    }
}

test "digits pass" {
    const parser = digits();

    switch (parser.run("1234")) {
        .success => |result| {
            try testing.expectEqualStrings("1234", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "digits fail" {
    const parser = digits();

    switch (parser.run("a1234")) {
        .success => |_| {
            try testing.expect(false);
        },
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expectEqualStrings("digit", e.expected);
            try testing.expectEqualStrings("a1234", e.found);
        },
    }
}

test "letters pass" {
    const parser = letters();

    switch (parser.run("ABCDEF")) {
        .success => |result| {
            try testing.expectEqualStrings("ABCDEF", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "letters fail" {
    const parser = letters();

    switch (parser.run("123456")) {
        .success => |_| {
            try testing.expect(false);
        },
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expectEqualStrings("letter", e.expected);
            try testing.expectEqualStrings("123456", e.found);
        },
    }
}

test "sequence with strings" {
    const parser = sequence(&.{ str("hello "), str("world") });
    switch (parser.run("hello world")) {
        .success => |result| {
            try testing.expectEqualStrings("hello world", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "failure sequence with strings and eof" {
    const parser = sequence(&.{ str("hello "), str("world"), eof() });
    switch (parser.run("hello world wow")) {
        .success => |_| {
            try testing.expect(false);
        },
        .err => |e| {
            try testing.expectEqual(@as(usize, 11), e.index);
            try testing.expectEqualStrings("end of input", e.expected);
            try testing.expectEqualStrings(" wow", e.found);
        },
    }
}

test "letters" {
    const parser = letters();
    switch (parser.run("helloworld")) {
        .success => |result| {
            try testing.expectEqualStrings("helloworld", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "lettersN success" {
    const parser = lettersN(5);
    switch (parser.run("helloworld")) {
        .success => |result| {
            try testing.expectEqualStrings("hello", result.value());
            try testing.expectEqual(@as(usize, 5), result.consumed);
        },
        .err => |_| try testing.expect(false),
    }
}

test "lettersN overflow" {
    const parser = lettersN(15);
    switch (parser.run("helloworld")) {
        .success => |_| try testing.expect(false),
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expect(std.mem.startsWith(u8, e.expected, "15"));
        },
    }
}

test "lettersN not enough letters" {
    const parser = lettersN(5);
    switch (parser.run("hel12")) {
        .success => |_| try testing.expect(false),
        .err => |e| {
            try testing.expectEqual(@as(usize, 3), e.index);
            try testing.expectEqualStrings("letter", e.expected);
        },
    }
}

test "digits" {
    const parser = digits();
    switch (parser.run("12345654321")) {
        .success => |result| {
            try testing.expectEqualStrings("12345654321", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "digitsN success" {
    const parser = digitsN(6);
    switch (parser.run("123123")) {
        .success => |result| {
            try testing.expectEqualStrings("123123", result.value());
            try testing.expectEqual(@as(usize, 6), result.consumed);
        },
        .err => |_| try testing.expect(false),
    }
}

test "digitsN overflow" {
    const parser = digitsN(15);
    switch (parser.run("12")) {
        .success => |_| try testing.expect(false),
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expect(std.mem.startsWith(u8, e.expected, "15"));
        },
    }
}

test "digitsN not enough letters" {
    const parser = digitsN(5);
    switch (parser.run("123AB")) {
        .success => |_| try testing.expect(false),
        .err => |e| {
            try testing.expectEqual(@as(usize, 3), e.index);
            try testing.expectEqualStrings("digit", e.expected);
        },
    }
}

test "choices" {
    const parser = choice(&.{ str("hello"), str("world") });
    switch (parser.run("hello")) {
        .success => |result| {
            try testing.expectEqualStrings("hello", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "choices fail" {
    const parser = choice(&.{ str("hello"), str("world") });
    switch (parser.run("alpha")) {
        .success => |_| {
            try testing.expect(false);
        },
        .err => |e| {
            try testing.expectEqual(@as(usize, 0), e.index);
            try testing.expectEqualStrings("hello", e.expected);
        },
    }
}

test "many" {
    const parser = many(&digitsN(1));
    switch (parser.run("12345")) {
        .success => |result| {
            try testing.expectEqualStrings("12345", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

test "not" {
    const parser = sequence(&.{ not(&str("XYZ")), letters() });
    switch (parser.run("ABC")) {
        .success => |result| {
            try testing.expectEqualStrings("ABC", result.value());
        },
        .err => |_| {
            try testing.expect(false);
        },
    }
}

fn quotedString() Parser {
    return sequence(&.{ str("\""), many(&sequence(&.{ not(&str("\"")), any() })), str("\"") });
}

test "lazy attribute success" {
    const parser = sequence(&.{ letters(), str("="), lazy(quotedString) });

    switch (parser.run("class=\"container\"")) {
        .success => |result| {
            try testing.expectEqualStrings("class=\"container\"", result.value());
        },
        .err => |_| try testing.expect(false),
    }
}
