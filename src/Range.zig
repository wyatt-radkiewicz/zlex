//! Represents a range of characters to match against
const std = @import("std");
const Self = @This();

start: []const u21,
end: []const u21,

/// Maximum number of ranges allowed when computing a range object
const max_ranges = 64;

pub const Error = error{
    InvalidFormat,
    OutOfBounds,
};

pub const ParseResult = struct {
    /// The range that was parsed
    range: Self,

    /// How big is was in the source
    len: usize,
};

/// Parses a range pattern
/// ---
/// - `src` source string (end of `src` may be past the end of the range).
///     `src` can *NOT* contain escape sequences, but may contain POSIX character classes and
///     raw utf-8 encoding. `src` may also point to a single character, in which it will make a
///     single-char range.
/// ---
/// returns:
/// - Either a `Error` or a sorted, non-overlapping `Range`
pub fn parse(comptime src: []const u8) Error!ParseResult {
    var invert = false;
    var len: usize = 0;
    const parsed = try parseRawRanges(src, &len, &invert);
    const sorted = sortRange(parsed);
    const coalessed = coalessRange(sorted);
    const range = if (invert) try invertRange(coalessed) else coalessed;
    return .{ .range = range, .len = len };
}

/// Inverts a sorted, non-overlapping range making sure create distinct, sorted sections
fn invertRange(range: Self) Error!Self {
    // Since the range covers the entire domain of u21, return nothing
    if (range.start[0] == 0 and range.end[0] == std.math.maxInt(u21)) {
        return .{ .start = &.{}, .end = &.{} };
    }

    var starts = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
    var ends = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;

    // If the first block starts at 1, start at the block after that one (hence idx = 1)
    var idx: usize = @intFromBool(range.start[0] == 0);
    var start: u21 = if (idx == 1) range.end[0] + 1 else 0;
    while (idx < range.start.len) : (idx += 1) {
        // Append the range and set the starting point for the next one
        starts.append(start) catch return Error.OutOfBounds;
        ends.append(range.start[idx] - 1) catch return Error.OutOfBounds;
        start = range.end[idx] +% 1;
    }

    // start will overlap to 0 if the final block ends at the end of the u21 domain. If that
    // doesn't happen, we add 1 more block to cover the end of the domain
    if (start != 0) {
        starts.append(start) catch return Error.OutOfBounds;
        ends.append(std.math.maxInt(u21)) catch return Error.OutOfBounds;
    }

    // Finalize the variables for comptime
    std.debug.assert(starts.len == ends.len);
    const final_starts = starts.buffer;
    const final_ends = ends.buffer;
    return .{ .start = final_starts[0..starts.len], .end = final_ends[0..starts.len] };
}

/// Computes a `Range` without overlapping ranges
fn coalessRange(sorted: Self) Self {
    // Now that we know the order we can start coallesing ranges
    var starts: [sorted.start.len]u21 = sorted.start[0..].*;
    var ends: [sorted.end.len]u21 = sorted.end[0..].*;

    var start = 0; // This is were we originally started this range
    var end = 1; // This is the last range we found that is apart of the starting one
    while (end < starts.len) : (end += 1) {
        if (starts[end] <= ends[start] +| 1) {
            // Extend the range if this one is bigger
            ends[start] = @max(ends[end], ends[start]);
        } else {
            // End the range (and start a new one)
            start += 1;
            starts[start] = starts[end];
            ends[start] = ends[end];
        }
    }

    const final_starts = starts;
    const final_ends = ends;
    return .{ .start = final_starts[0 .. start + 1], .end = final_ends[0 .. start + 1] };
}

/// Computes an ordered `Range` from a unordered `Range` (ranges may still overlap) by starting
/// position in the ranges
fn sortRange(unsorted: Self) Self {
    // Remove overlapping ranges by starting with sorting them
    var order: [unsorted.start.len]usize = undefined;
    for (0..order.len) |i| {
        order[i] = i;
    }
    const SortContext = struct {
        starts: []const u21,
        pub fn lessThanFn(ctx: @This(), lhs: usize, rhs: usize) bool {
            return ctx.starts[lhs] < ctx.starts[rhs];
        }
    };
    std.mem.sortUnstable(usize, &order, SortContext{
        .starts = unsorted.start,
    }, SortContext.lessThanFn);

    // Create ordered lists from the sorting algo.
    var order_starts: [order.len]u21 = undefined;
    var order_ends: [order.len]u21 = undefined;
    var order_len = 0;
    for (order) |i| {
        order_starts[order_len] = unsorted.start[i];
        order_ends[order_len] = unsorted.end[i];
        order_len += 1;
    }

    const final_starts = order_starts;
    const final_ends = order_ends;
    return .{ .start = &final_starts, .end = &final_ends };
}

/// Only return the ranges encoded verbatim, they may be overlapping, or even out of order
/// ---
/// returns:
/// - A tuple containing a `bool` to specify whether or not the `Range` should be inverted later
fn parseRawRanges(comptime src: []const u8, idx: *usize, invert: *bool) Error!Self {
    // Make sure that we are starting with a range, if not, read it as a normal character
    if (src[0] != '[') {
        return switch (try readChar(src, idx)) {
            '.' => Self{ .start = &.{0}, .end = &.{std.math.maxInt(u21)} },
            else => |c| Self{ .start = &.{c}, .end = &.{c} },
        };
    }

    // Create arrays for the start and end of each range found
    var starts = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
    var ends = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;

    // Parse the source
    idx.* = 1;
    if (src[idx.*] == ':') {
        // Parse character classes (classes must be surrounded in [:class:])
        idx.* += 1;

        // This is where we start the character class name
        const start = idx.*;
        while (idx.* < src.len and src[idx.*] != ':') : (idx.* += 1) {}
        if (src[idx.*] != ':') return Error.InvalidFormat;

        // Look up the character class and add it to the list
        const ranges = (try Class.parse(src[start..idx.*])).getRanges();
        var range: usize = 0;
        while (range < ranges.len) : (range += 2) {
            starts.append(ranges[range + 0]) catch return Error.OutOfBounds;
            ends.append(ranges[range + 1]) catch return Error.OutOfBounds;
        }
        idx.* += 1;
    } else {
        invert.* = src[idx.*] == '^';
        if (invert.*) idx.* += 1;

        // Parse ranges
        while (src[idx.*] != ']') {
            // Its not a character range, but this is including another range
            if (src[idx.*] == '[') {
                const result = try Self.parse(src[idx.*..]);
                idx.* += result.len;
                starts.appendSlice(result.range.start) catch return Error.OutOfBounds;
                ends.appendSlice(result.range.end) catch return Error.OutOfBounds;
                continue;
            }

            // Read in starting character
            const start = try readChar(src, &idx.*);
            starts.append(start) catch return Error.OutOfBounds;

            // Its a single character range, so skip it
            if (src[idx.*] != '-') {
                ends.append(start) catch return Error.OutOfBounds;
                continue;
            } else {
                idx.* += 1;
            }

            // Read in the ending character of the range
            const end = try readChar(src, &idx.*);
            ends.append(end) catch Error.OutOfBounds;
            if (start > end) {
                // Reverse the range since they put it in backwards :/
                std.mem.swap(u21, &starts.buffer[starts.len - 1], &ends.buffer[ends.len - 1]);
            }
        }
    }

    // Make sure we have a correctly formatted range
    std.debug.assert(starts.len == ends.len);
    if (starts.len == 0 or src[idx.*] != ']') return Error.InvalidFormat;
    idx.* += 1;

    // Finalize the variables for comptime
    const final_starts = starts.buffer;
    const final_ends = ends.buffer;
    return .{
        .start = final_starts[0..starts.len],
        .end = final_ends[0..starts.len],
    };
}

/// Returns true if the range can be cut to only include ascii characters and not lose
/// important data
pub fn isAscii(self: Self) bool {
    // Since ranges are sorted only look at the final range
    const start = self.start[self.start.len - 1];
    const end = self.end[self.end.len - 1];
    return start <= 0x80 and end == std.math.maxInt(u21) or end < 0x80;
}

/// Helper function to read a UTF8 character
fn readChar(comptime src: []const u8, i: *usize) Error!u21 {
    // Escape any character basically
    if (src[i.*] == '\\') i.* += 1;
    const n = std.unicode.utf8ByteSequenceLength(src[i.*]) catch return Error.InvalidFormat;
    const c = std.unicode.utf8Decode(src[i.*..][0..n]) catch return Error.InvalidFormat;
    i.* += n;
    return c;
}

/// A character class (names match)
const Class = enum {
    alnum,
    alpha,
    ascii,
    blank,
    cntrl,
    digit,
    graph,
    lower,
    print,
    punct,
    space,
    upper,
    word,
    xdigit,

    // Tries to get the character class from a string
    // - `str` string of the class name *only*. It should end at the end of the name.
    pub fn parse(str: []const u8) Error!Class {
        return std.meta.stringToEnum(Class, str) orelse return Error.InvalidFormat;
    }

    /// Returns an array of [start, end, start, end, ...]
    pub fn getRanges(comptime self: Class) []const u21 {
        return switch (self) {
            .alnum => &[_]u21{ '0', '9', 'A', 'Z', 'a', 'z' },
            .alpha => &[_]u21{ 'A', 'Z', 'a', 'z' },
            .ascii => &[_]u21{ 0x00, 0x7F },
            .blank => &[_]u21{ ' ', ' ', '\t', '\t' },
            .cntrl => &[_]u21{ 0x00, 0x1F, 0x7F },
            .digit => &[_]u21{ '0', '9' },
            .graph => &[_]u21{ '!', '~' },
            .lower => &[_]u21{ 'a', 'z' },
            .print => &[_]u21{ ' ', '~' },
            .punct => &[_]u21{ '!', '/', ':', '@', '[', '`', '{', '~' },
            .space => &[_]u21{ 0x09, 0x0D, ' ', ' ' },
            .upper => &[_]u21{ 'A', 'Z' },
            .word => &[_]u21{ '0', '9', 'A', 'Z', '_', '_', 'a', 'z' },
            .xdigit => &[_]u21{ '0', '9', 'A', 'F', 'a', 'f' },
        };
    }
};

test "Range" {
    const max = std.math.maxInt(u21);

    // Test multiple values
    var result = comptime try Self.parse("[abc]---");
    try std.testing.expectEqualSlices(u21, &.{'a'}, result.range.start);
    try std.testing.expectEqualSlices(u21, &.{'c'}, result.range.end);
    try std.testing.expectEqual(5, result.len);
    try std.testing.expectEqual(true, result.range.isAscii());

    // Test negation
    result = comptime try Self.parse("[^abc]other stuff");
    try std.testing.expectEqualSlices(u21, &.{ 0x00, 'd' }, result.range.start);
    try std.testing.expectEqualSlices(u21, &.{ '`', max }, result.range.end);
    try std.testing.expectEqual(6, result.len);
    try std.testing.expectEqual(true, result.range.isAscii());

    // Test minus and reversed range
    result = comptime try Self.parse("[--+]");
    try std.testing.expectEqualSlices(u21, &.{'+'}, result.range.start);
    try std.testing.expectEqualSlices(u21, &.{'-'}, result.range.end);
    try std.testing.expectEqual(5, result.len);
    try std.testing.expectEqual(true, result.range.isAscii());

    // Test character group
    result = comptime try Self.parse("[:alnum:]");
    try std.testing.expectEqualSlices(u21, &.{ '0', 'A', 'a' }, result.range.start);
    try std.testing.expectEqualSlices(u21, &.{ '9', 'Z', 'z' }, result.range.end);
    try std.testing.expectEqual(9, result.len);
    try std.testing.expectEqual(true, result.range.isAscii());

    // Test character group inversion
    result = comptime try Self.parse("[^[:alnum:]]");
    try std.testing.expectEqualSlices(u21, &.{ 0x00, ':', '[', '{' }, result.range.start);
    try std.testing.expectEqualSlices(u21, &.{ '/', '@', '`', max }, result.range.end);
    try std.testing.expectEqual(12, result.len);
    try std.testing.expectEqual(true, result.range.isAscii());
}
