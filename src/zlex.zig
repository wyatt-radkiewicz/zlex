const std = @import("std");
const Type = std.builtin.Type;

/// Generates a lexer iterator with a method that returns an union defining the matched token
/// ---
/// Parameters:
/// - `Space` defines a type to just put some state for the lexer actions. By default
/// the lexer already comes with some bear bones state. This includes:
///   - Line and column in the text
/// - `tokens` describes a tuple of matching patterns, each corresponding to a variant
/// in the token type returned by the lexer type.
/// ---
/// Example:
/// ```zig
/// Lexer(void, .{
///     .variant_with_void_type = "matching_pattern",
///     .plus_eq = "+=",
///     .variant_with_type_or_action = struct {
///         pub const capture = "(0|([1-9][0-9]*))(.[0-9]+)?";
///
///         // You can omit this if you like, which will make this equivalent to the above patterns
///         // You can also choose not to return an error if you don't need one (less branching!)
///         pub fn action(state: *State, match: []const u8) !f64 {
///             return std.fmt.parseFloat(f64, match);
///         }
///     },
///     .whitespace = struct {
///         pub const skip = "[ \t\n]+";
///
///         // You can ommit this as well like above
///         pub fn action(state: *State, match: []const u8) void {
///             // Even though the whitespace should be skipped you can still see what matched here
///             _ = state;
///             _ = match;
///         }
///     },
/// }).init(state_object, src_string)
/// ```
pub fn Lexer(comptime State: type, comptime tokens: anytype) type {
    return struct {
        state: State,
        
        const Self = @This();
        pub const Token = TokenUnion(TokenTag(tokens), tokens);
    };
}

// Generates an enum, every variant corresponding to a struct field passed in
// ---
// - `tokens` generate a unique variant for each member of the anonymous struct
fn TokenTag(comptime tokens: anytype) type {
    const fields = std.meta.fields(@TypeOf(tokens));
    var variants: [fields.len]Type.EnumField = undefined;
    for (fields, 0..) |field, idx| {
        variants[idx] = .{ .name = field.name, .value = idx };
    }

    return @Type(Type{ .Enum = .{
        .tag_type = std.math.IntFittingRange(0, fields.len),
        .fields = &variants,
        .decls = &.{},
        .is_exhaustive = false,
    } });
}

/// Generates a union with every variant corresponding to a struct field passed in
/// ---
/// - `Tag` the tag for the generated union to use (must have same variant names as fields's fields)
/// - `fields` fields to generate variants for, with same names as variants in Tag
fn TokenUnion(comptime Tag: type, comptime tokens: anytype) type {
    const tag_variants = std.meta.fields(Tag);
    var union_variants: [tag_variants.len]Type.UnionField = undefined;
    for (tag_variants, &union_variants) |tag_variant, *union_variant| {
        // This is the real value of the field, which may be a `str` or a `type` refrencing a struct
        const field = @field(tokens, tag_variant.name);

        // If the type of it is a `type` and that type has a function `action`, get it's return type
        const VariantType = if (@TypeOf(field) == type and @hasDecl(field, "action"))
            switch (@typeInfo(@TypeOf(field.action))) {
                // If its an error union, get its underlying type
                .Fn => |fn_info| switch (@typeInfo(fn_info.return_type orelse void)) {
                    .ErrorUnion => |return_info| return_info.payload,
                    else => fn_info.return_type orelse void,
                },
                else => void,
            }
        else
            void;

        // Use the found type to generate the union variant
        union_variant.* = .{
            .name = tag_variant.name,
            .type = VariantType,
            .alignment = @alignOf(VariantType),
        };
    }

    return @Type(Type{ .Union = .{
        .layout = .auto,
        .tag_type = Tag,
        .fields = &union_variants,
        .decls = &.{},
    } });
}

/// Represents a range of characters to match against
const Range = struct {
    /// Where this was parsed from
    src: []const u8,

    /// Where the ranges start (must be same length as end)
    start: []const u21,

    /// Where the ranges end (inclusive) (must be same length as start)
    end: []const u21,
    
    /// Maximum number of ranges allowed when computing a range object
    const max_ranges = 64;

    /// Parses a range pattern
    /// ---
    /// - `src` source string (end of `src` may be past the end of the range).
    ///     `src` can *NOT* contain escape sequences, but may contain POSIX character classes and
    ///     raw utf-8 encoding.
    /// ---
    /// returns:
    /// - Either a `ParseError` or a sorted, non-overlapping `Range`
    pub fn parse(comptime src: []const u8) ParseError!Range {
        const parsed = try parseRawRanges(src);
        const sorted = sortRange(parsed[1]);
        const coalessed = coalessRange(sorted);
        return if (parsed[0]) try invertRange(coalessed) else coalessed;
    }

    /// Inverts a sorted, non-overlapping range making sure create distinct, sorted sections
    fn invertRange(comptime range: Range) ParseError!Range {
        // Since the range covers the entire domain of u21, return nothing
        if (range.start[0] == 0 and range.end[0] == std.math.maxInt(u21)) {
            return .{
                .src = range.src,
                .start = &.{},
                .end = &.{},
            };
        }
    
        var starts = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
        var ends = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
        
        // If the first block starts at 1, start at the block after that one (hence idx = 1)
        var idx: usize = @intFromBool(range.start[0] == 0);
        var start: u21 = if (idx == 1) range.end[0] + 1 else 0;
        while (idx < range.start.len) : (idx += 1) {
            // Append the range and set the starting point for the next one
            starts.append(start) catch return ParseError.OutOfMemory;
            ends.append(range.start[idx] - 1) catch return ParseError.OutOfMemory;
            start = range.end[idx] +% 1;
        }
        
        // start will overlap to 0 if the final block ends at the end of the u21 domain. If that
        // doesn't happen, we add 1 more block to cover the end of the domain
        if (start != 0) {
            starts.append(start) catch return ParseError.OutOfMemory;
            ends.append(std.math.maxInt(u21)) catch return ParseError.OutOfMemory;
        }
        
        // Finalize the variables for comptime
        std.debug.assert(starts.len == ends.len);
        const final_starts = starts.buffer;
        const final_ends = ends.buffer;
        return .{
            .src = range.src,
            .start = final_starts[0..starts.len],
            .end = final_ends[0..starts.len],
        };
    }
    
    /// Computes a `Range` without overlapping ranges
    fn coalessRange(comptime sorted: Range) Range {
        // Now that we know the order we can start coallesing ranges
        var starts: [sorted.start.len]u21 = undefined;
        var ends: [sorted.end.len]u21 = undefined;
        @memcpy(&starts, sorted.start);
        @memcpy(&ends, sorted.end);

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
        return .{
            .src = sorted.src,
            .start = final_starts[0 .. start + 1],
            .end = final_ends[0 .. start + 1],
        };
    }

    /// Computes an ordered `Range` from a unordered `Range` (ranges may still overlap) by starting
    /// position in the ranges
    fn sortRange(comptime unsorted: Range) Range {
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
        return .{
            .src = unsorted.src,
            .start = &final_starts,
            .end = &final_ends,
        };
    }

    /// Only return the ranges encoded verbatim, they may be overlapping, or even out of order
    /// ---
    /// returns:
    /// - A tuple containing a `bool` to specify whether or not the `Range` should be inverted later
    fn parseRawRanges(comptime src: []const u8) ParseError!struct { bool, Range } {
        // Make sure that we are starting with a range, and set the max number of ranges we accept
        if (src[0] != '[') return ParseError.InvalidRangeFormat;

        // Create arrays for the start and end of each range found
        var starts = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
        var ends = std.BoundedArray(u21, max_ranges).init(0) catch unreachable;
        var negate = false;

        // Parse the source
        var idx: usize = 1;
        if (src[idx] == ':') {
            // Parse character classes (classes must be surrounded in [:class:])
            idx += 1;

            // This is where we start the character class name
            const start = idx;
            while (idx < src.len and src[idx] != ':') : (idx += 1) {}
            if (src[idx] != ':') return ParseError.InvalidRangeFormat;

            // Look up the character class and add it to the list
            const ranges = (try Class.parse(src[start..idx])).getRanges();
            var range: usize = 0;
            while (range < ranges.len) : (range += 2) {
                starts.append(ranges[range + 0]) catch return ParseError.OutOfMemory;
                ends.append(ranges[range + 1]) catch return ParseError.OutOfMemory;
            }
            idx += 1;
        } else {
            negate = src[idx] == '^';
            if (negate) idx += 1;

            // Parse ranges
            while (src[idx] != ']') {
                // Its not a character range, but this is including another range
                if (src[idx] == '[') {
                    const range = try Range.parse(src[idx..]);
                    idx += range.src.len;
                    for (range.start, range.end) |start, end| {
                        starts.append(start) catch return ParseError.OutOfMemory;
                        ends.append(end) catch return ParseError.OutOfMemory;
                    }
                    continue;
                }

                // Read in starting character
                const start = try readChar(src, &idx);
                starts.append(start) catch return ParseError.OutOfMemory;

                // Its a single character range, so skip it
                if (src[idx] != '-') {
                    ends.append(start) catch return ParseError.OutOfMemory;
                    continue;
                } else {
                    idx += 1;
                }

                // Read in the ending character of the range
                const end = try readChar(src, &idx);
                ends.append(end) catch ParseError.OutOfMemory;
                if (start > end) {
                    // Reverse the range since they put it in backwards :/
                    std.mem.swap(u21, &starts.buffer[starts.len - 1], &ends.buffer[ends.len - 1]);
                }
            }
        }

        // Make sure we have a correctly formatted range
        std.debug.assert(starts.len == ends.len);
        if (starts.len == 0 or src[idx] != ']') return ParseError.InvalidRangeFormat;
        idx += 1;

        // Finalize the variables for comptime
        const final_starts = starts.buffer;
        const final_ends = ends.buffer;
        return .{
            negate,
            .{
                .src = src[0 .. idx],
                .start = final_starts[0..starts.len],
                .end = final_ends[0..starts.len],
            }
        };
    }

    /// Returns true if the range can be cut to only include ascii characters and not lose
    /// important data
    pub fn isAscii(self: Range) bool {
        // Since ranges are sorted only look at the final range
        const start = self.start[self.start.len - 1];
        const end = self.end[self.end.len - 1];
        return start <= 0x80 and end == std.math.maxInt(u21) or end < 0x80;
    }

    /// Helper function to read a UTF8 character
    fn readChar(comptime src: []const u8, i: *usize) ParseError!u21 {
        const n = std.unicode.utf8ByteSequenceLength(src[i.*]) catch return ParseError.InvalidUTF8;
        const c = std.unicode.utf8Decode(src[i.*..][0..n]) catch return ParseError.InvalidUTF8;
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
        pub fn parse(str: []const u8) ParseError!Class {
            return std.meta.stringToEnum(Class, str) orelse return ParseError.InvalidRangeFormat;
        }

        /// Returns an array of [start, end, start, end, ...]
        pub fn getRanges(comptime self: Class) []const u21 {
            return switch (self) {
                .alnum => &[_]u21{ 'a', 'z', 'A', 'Z', '0', '9' },
                .alpha => &[_]u21{ 'a', 'z', 'A', 'Z' },
                .ascii => &[_]u21{ 0x00, 0x7F },
                .blank => &[_]u21{ ' ', ' ', '\t', '\t' },
                .cntrl => &[_]u21{ 0x00, 0x1F, 0x7F },
                .digit => &[_]u21{ '0', '9' },
                .graph => &[_]u21{ '!', '~' },
                .lower => &[_]u21{ 'a', 'z' },
                .print => &[_]u21{ ' ', '~' },
                .punct => &[_]u21{ '!', '/', ':', '@', '[', '`', '{', '~' },
                .space => &[_]u21{ ' ', ' ', 0x09, 0x0D },
                .upper => &[_]u21{ 'A', 'Z' },
                .word => &[_]u21{ 'a', 'z', 'A', 'Z', '0', '9', '_', '_' },
                .xdigit => &[_]u21{ '0', '9', 'a', 'f', 'A', 'F' },
            };
        }
    };
};

/// This is an update

/// Errors that can occur when parsing regex
pub const ParseError = error{
    InvalidRangeFormat,
    InvalidUTF8,
    OutOfMemory,
};

test "Token" {
    const lexer = Lexer(void, .{
        .digit = struct {
            pub const capture = "[0-9]";
            pub fn action(_: void, match: []const u8) !u4 {
                return match[0] - '0';
            }
        },
        .ident = "[a-zA-Z_][a-zA-Z0-9_]*",
        .plus = "+",
        .minus = "-",
    });

    const tags = std.meta.fields(std.meta.Tag(lexer.Token));
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "digit",
        .value = 0,
    }, tags[0]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "ident",
        .value = 1,
    }, tags[1]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "plus",
        .value = 2,
    }, tags[2]);
    try std.testing.expectEqualDeep(Type.EnumField{
        .name = "minus",
        .value = 3,
    }, tags[3]);

    const fields = std.meta.fields(lexer.Token);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "digit",
        .type = u4,
        .alignment = 1,
    }, fields[0]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "ident",
        .type = void,
        .alignment = 1,
    }, fields[1]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "plus",
        .type = void,
        .alignment = 1,
    }, fields[2]);
    try std.testing.expectEqualDeep(Type.UnionField{
        .name = "minus",
        .type = void,
        .alignment = 1,
    }, fields[3]);
}

test "Range" {
    const max = std.math.maxInt(u21);

    // Test multiple values
    var range = comptime try Range.parse("[abc]---");
    try std.testing.expectEqualSlices(u21, &.{'a'}, range.start);
    try std.testing.expectEqualSlices(u21, &.{'c'}, range.end);
    try std.testing.expectEqualSlices(u8, "[abc]", range.src);
    try std.testing.expectEqual(true, range.isAscii());

    // Test negation
    range = comptime try Range.parse("[^abc]other stuff");
    try std.testing.expectEqualSlices(u21, &.{ 0x00, 'd' }, range.start);
    try std.testing.expectEqualSlices(u21, &.{ '`', max }, range.end);
    try std.testing.expectEqualSlices(u8, "[^abc]", range.src);
    try std.testing.expectEqual(true, range.isAscii());
    
    // Test minus and reversed range
    range = comptime try Range.parse("[--+]");
    try std.testing.expectEqualSlices(u21, &.{ '+' }, range.start);
    try std.testing.expectEqualSlices(u21, &.{ '-' }, range.end);
    try std.testing.expectEqualSlices(u8, "[--+]", range.src);
    try std.testing.expectEqual(true, range.isAscii());
    
    // Test character group
    range = comptime try Range.parse("[:alnum:]");
    try std.testing.expectEqualSlices(u21, &.{ '0', 'A', 'a' }, range.start);
    try std.testing.expectEqualSlices(u21, &.{ '9', 'Z', 'z' }, range.end);
    try std.testing.expectEqualSlices(u8, "[:alnum:]", range.src);
    try std.testing.expectEqual(true, range.isAscii());
    
    // Test character group inversion
    range = comptime try Range.parse("[^[:alnum:]]");
    try std.testing.expectEqualSlices(u21, &.{ 0x00, ':', '[', '{' }, range.start);
    try std.testing.expectEqualSlices(u21, &.{ '/', '@', '`', max }, range.end);
    try std.testing.expectEqualSlices(u8, "[^[:alnum:]]", range.src);
    try std.testing.expectEqual(true, range.isAscii());
}
