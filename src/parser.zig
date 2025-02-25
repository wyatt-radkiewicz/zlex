//! Parses a regex into a abstract syntax tree
const std = @import("std");
const Range = @import("Range.zig");
pub const Error = Range.Error;

pub const Result = union(enum) {
    err: Err,
    ok: *const Node,

    pub const Err = struct {
        err: Error,
        at: []const u8,
    };
};

pub const Node = union(enum) {
    symbol: Range,
    zero_many: *const Node,
    one_many: *const Node,
    optional: *const Node,
    either: Split,
    sequence: Split,

    pub const Split = struct {
        left: *const Node,
        right: *const Node,
    };
};

pub const Prec = enum {
    alternation,
    sequence,
    factor,

    pub fn underLevel(self: Prec, level: Prec) bool {
        return @intFromEnum(self) <= @intFromEnum(level);
    }

    pub fn lowest() Prec {
        return .alternation;
    }

    pub fn next(self: Prec) Prec {
        return switch (self) {
            else => @enumFromInt(@intFromEnum(self) + 1),
            .factor => .factor,
        };
    }
};

pub fn parse(comptime src: []const u8) Result {
    var curr = src;
    if (parseExpr(&curr, Prec.lowest())) |ast| {
        return if (curr.len == 0) .{ .ok = ast } else .{ .err = .{
            .err = Error.InvalidFormat,
            .at = curr,
        } };
    } else |err| {
        return .{ .err = .{ .err = err, .at = curr } };
    }
}

fn parseExpr(comptime src: *[]const u8, prec: Prec) Error!*const Node {
    // Every regex starts with a prefix node/instruction
    var prefix = try parsePrefix(src);

    // Now we parse nodes that require previous nodes
    while (try parseSuffix(src, prefix, prec)) |next| {
        prefix = next;
    }

    return prefix;
}

fn parsePrefix(comptime src: *[]const u8) Error!*const Node {
    if (src.*[0] == '(') {
        src.* = src.*[1..];
        return parseExpr(src, Prec.lowest());
    } else {
        // Parse a symbol (terminal)
        const range = try Range.parse(src.*);
        src.* = src.*[range.len..];
        return &.{ .symbol = range.range };
    }
}

fn parseSuffix(comptime src: *[]const u8, left: *const Node, prec: Prec) Error!?*const Node {
    if (src.*.len == 0) return null;
    switch (src.*[0]) {
        '+' => if (prec.underLevel(.factor)) {
            src.* = src.*[1..];
            return &.{ .one_many = left };
        },
        '*' => if (prec.underLevel(.factor)) {
            src.* = src.*[1..];
            return &.{ .zero_many = left };
        },
        '?' => if (prec.underLevel(.factor)) {
            src.* = src.*[1..];
            return &.{ .optional = left };
        },
        '|' => if (prec.underLevel(.alternation)) {
            src.* = src.*[1..];
            return &.{ .either = .{
                .left = left,
                .right = try parseExpr(src, Prec.alternation.next()),
            } };
        },
        ')' => src.* = src.*[1..],
        else => if (prec.underLevel(.sequence)) {
            return &.{ .sequence = .{
                .left = left,
                .right = try parseExpr(src, Prec.sequence.next()),
            } };
        },
    }

    return null;
}

test "parser" {
    try std.testing.expectEqualDeep(Result{ .ok = &Node{ .sequence = .{
        .left = &Node{ .sequence = .{
            .left = &Node{ .symbol = Range{
                .start = &.{'a'},
                .end = &.{'a'},
            } },
            .right = &Node{ .symbol = Range{
                .start = &.{'b'},
                .end = &.{'b'},
            } },
        } },
        .right = &Node{ .symbol = Range{
            .start = &.{'c'},
            .end = &.{'c'},
        } },
    } } }, comptime parse("abc"));
    try std.testing.expectEqualDeep(Result{ .ok = &Node{ .one_many = &Node{ .either = .{
        .left = &Node{ .sequence = .{
            .left = &Node{ .sequence = .{
                .left = &Node{ .symbol = Range{
                    .start = &.{'a'},
                    .end = &.{'a'},
                } },
                .right = &Node{ .symbol = Range{
                    .start = &.{'b'},
                    .end = &.{'b'},
                } },
            } },
            .right = &Node{ .symbol = Range{
                .start = &.{'c'},
                .end = &.{'c'},
            } },
        } },
        .right = &Node{ .zero_many = &Node{ .symbol = Range{
            .start = &.{'d'},
            .end = &.{'d'},
        } } },
    } } } }, comptime parse("((abc)|d*)+"));
    try std.testing.expectEqualDeep(Result{ .ok = &Node{ .sequence = .{
        .left = &Node{ .symbol = Range{
            .start = &.{'a'},
            .end = &.{'a'},
        } },
        .right = &Node{ .zero_many = &Node{ .symbol = Range{
            .start = &.{'b'},
            .end = &.{'b'},
        } } },
    } } }, comptime parse("ab*"));
}
