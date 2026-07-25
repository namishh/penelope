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
            try writer.print("parse error at byte {d}: expected {s}, found '{s}'", .{ self.index, self.expected, self.found });
        } else {
            try writer.print("parse error at byte {d}: expected {s}, found end of input", .{ self.index, self.expected });
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

            .sequence => |parsers| {
                const checkpoint = state.index;
                for (parsers) |parser| {
                    _ = parser.parse(state) catch |err| {
                        state.index = checkpoint;
                        return err;
                    };
                }
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

test "failure sequence with strings" {
    const parser = sequence(&.{ str("hello "), str("world") });
    switch (parser.run("hello wold")) {
        .success => |_| {
            try testing.expect(false);
        },
        .err => |e| {
            try testing.expectEqual(@as(usize, 6), e.index);
            try testing.expectEqualStrings("world", e.expected);
            try testing.expectEqualStrings("wold", e.found);
        },
    }
}
