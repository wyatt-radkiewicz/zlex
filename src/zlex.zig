const std = @import("std");
const parser = @import("parser.zig");
const token = @import("token.zig");
const nfa = @import("nfa.zig");
const dfa = @import("dfa.zig");

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
/// Lexer(void, struct{
///     // Defaults to void type
///     .variant_with_void_type = "matching_pattern",
///     .plus_eq = "+=",
///     .number = struct {
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
pub fn Lexer(comptime ContextParam: type, comptime tokens: anytype) type {
    // Create the multiple NFA's for every token
    const TokenType = token.Token(tokens);
    const token_info = tokensInfo(TokenType, tokens);
    const main_dfa = generateDfa(token_info);

    return struct {
        context: Context,
        src: []const u8,
        line: usize,
        col: usize,

        const Self = @This();
        const State = CompressedState(main_dfa);
        pub const fa = compress(main_dfa);
        pub const Context = ContextParam;
        pub const Token = TokenType;

        /// Creates a new lexer instance for this src string.
        /// This function only copies context state, and the src pointer. Very simple
        pub fn init(context: Context, src: []const u8) Self {
            return .{ .context = context, .src = src, .line = 1, .col = 1 };
        }

        /// This function finds the next token. If at the end of the source string, return null
        pub fn next(self: *Self) anyerror!?Token {
            return while (self.src.len != 0) {
                const start = self.src;
                var state: State.Trans = fa[0].trans[State.classes[self.src[0]]];
                while (state < State.accept_states) {
                    self.src = self.src[1..];
                    state = fa[state].trans[
                        State.classes[
                            if (self.src.len > 0) self.src[0] else 0
                        ]
                    ];
                }

                // Run the action to get the actual token value
                switch (state) {
                    inline State.accept_states...State.accept_states + token_info.len - 1 => |i| {
                        const id = i - State.accept_states;
                        const src = start[0 .. @intFromPtr(self.src.ptr) - @intFromPtr(start.ptr)];
                        if (token_info[id].skip) {
                            _ = try self.tokenAction(token_info[id], src);
                        } else {
                            break self.tokenAction(token_info[id], src);
                        }
                    },
                    else => break error.InvalidToken,
                }
            } else null;
        }

        fn tokenAction(
            self: *Self,
            comptime info: token.Info,
            match: []const u8,
        ) anyerror!?Token {
            is_void: {
                const s = @field(tokens, info.name);
                if (@TypeOf(s) != type) break :is_void;
                if (!@hasDecl(s, "action")) break :is_void;

                switch (@typeInfo(@TypeOf(s.action))) {
                    .Fn => |fn_info| switch (@typeInfo(fn_info.return_type.?)) {
                        .ErrorUnion => return if (ContextParam == void)
                            @unionInit(Token, info.name, try s.action({}, match))
                        else
                            @unionInit(Token, info.name, try s.action(&self.*.context, match)),
                        else => return if (ContextParam == void)
                            @unionInit(Token, info.name, s.action({}, match))
                        else
                            @unionInit(Token, info.name, s.action(&self.*.context, match)),
                    },
                    else => @compileError("zlex: expected action to be a function"),
                }
            }

            return @unionInit(Token, info.name, {});
        }
    };
}

/// Returns token.Info structs for every entry
fn tokensInfo(comptime Token: type, comptime tokens: anytype) []const token.Info {
    const token_fields = std.meta.fields(Token);
    var infos: [token_fields.len]token.Info = undefined;
    inline for (token_fields, &infos) |field, *info| {
        info.* = token.Info.init(@field(tokens, field.name), field.name);
    }
    const final = infos;
    return &final;
}

/// Generates a normal dfa from a tokens list and token type
fn generateDfa(comptime infos: []const token.Info) []const dfa.State {
    var token_nfas: [infos.len][]const nfa.State = undefined;
    inline for (infos, &token_nfas, 0..) |info, *fa, id| {
        const result = parser.parse(info.pattern);
        if (result == .err) @compileError(result.err);
        fa.* = nfa.construct(result.ok, id);
    }
    const main_nfa = nfa.combine(&token_nfas);
    return dfa.convert(main_nfa);
}

/// Generates the transition classes for 1 state's transitions
fn stateTransClass(comptime fa: []const dfa.State, state: usize) []const std.StaticBitSet(256) {
    @setEvalBranchQuota(1024 * 256);

    // Set class sets for every transition
    var classes = [1]std.StaticBitSet(256){std.StaticBitSet(256).initEmpty()} ** fa.len;
    for (fa[state].trans, 0..) |maybe_trans, idx| {
        classes[maybe_trans orelse continue].set(idx);
    }

    // Gather only set classes
    var non_empty = std.BoundedArray(std.StaticBitSet(256), fa.len).init(0) catch unreachable;
    for (classes) |class| {
        if (class.count() == 0) continue;
        non_empty.appendAssumeCapacity(class);
    }

    // Return final class list
    const final = non_empty.slice()[0..].*;
    return &final;
}

/// Makes a state structure that can hold the data specifically for one dfa
/// This structure also holds a conversion table from byte to transition type to reduce
/// dfa size on most lexer inputs
fn CompressedState(comptime fa: []const dfa.State) type {
    // Get all unique transition classes (this may be an almost infinite amount, but bound it)
    @setEvalBranchQuota(1024 * 2048 * 256);
    var all_classes = std.BoundedArray(std.StaticBitSet(256), 2048).init(0) catch unreachable;
    for (0..fa.len) |state| {
        const classes = stateTransClass(fa, state);

        // Add the classes if they are unique
        for (classes) |class| {
            if (for (all_classes.slice()) |other_class| {
                if (other_class.eql(class)) break false;
            } else true) {
                all_classes.appendAssumeCapacity(class);
            }
        }
    }

    // To generate the transition table we generate a hash for the permutations that come up during
    // the generation of the transitions
    @setEvalBranchQuota(256 * 2048 * 256);
    var trans_table = [1][256 / 8]u8{undefined} ** 256;
    for (0..256) |c| {
        var hash = std.crypto.hash.sha2.Sha256.init(.{});
        const writer = hash.writer();

        // Get the permutation by writing the indexes it is a part of to a sha256 hash
        for (all_classes.slice(), 0..) |class, idx| {
            if (class.isSet(c)) writer.writeInt(usize, idx, .little) catch @panic("zlex: SHA err");
        }

        // Now set the trans_table entry to the hash
        trans_table[c] = hash.finalResult();
    }

    // The with the hashes we give each a unique ID counting up from 1 (null being set to 0)
    @setEvalBranchQuota(256 * 256 * 256);
    var unique = std.BoundedArray([256 / 8]u8, 256).init(0) catch unreachable;
    for (trans_table) |hash| {
        if (for (unique.slice()) |unique_hash| {
            if (std.mem.eql(u8, &hash, &unique_hash)) break false;
        } else true) {
            unique.appendAssumeCapacity(hash);
        }
    }

    // Now using the unique array to convert hash to index, we can create the final transition table
    // and transition type
    @setEvalBranchQuota(256 * 256 * 256);
    const final_nclasses = unique.len + 1;
    const ClassType = std.math.IntFittingRange(0, final_nclasses - 1);
    var trans_classes: [256]ClassType = undefined;
    for (0..256) |c| {
        for (unique.slice(), 0..) |hash, idx| {
            if (std.mem.eql(u8, &hash, &trans_table[c])) {
                trans_classes[c] = idx;
                break;
            }
        } else {
            trans_classes[c] = unique.len;
        }
    }
    const final_classes = trans_classes;

    // In these compressed states, the transition also specifies the accept node so calculate number
    // of final states as well
    var max_final = 0;
    for (fa) |state| {
        if (state.token) |tok| {
            if (tok > max_final) max_final = tok;
        }
    }
    const final_err_state = fa.len + max_final;
    const final_accept_states = fa.len;
    const TransType = std.math.IntFittingRange(0, final_err_state);
    const final_ntrans = 1 << std.math.log2_int_ceil(usize, final_nclasses);

    // Now generate the type information
    return struct {
        pub const Class = ClassType;
        pub const Trans = TransType;
        pub const classes = final_classes;
        pub const ntrans = final_nclasses;
        pub const err_state = final_err_state;
        pub const accept_states = final_accept_states;

        trans: [final_ntrans]Trans,
    };
}

/// Compress a dfa to make it useable for the lexer in its smallest form
fn compress(comptime fa: []const dfa.State) []const CompressedState(fa) {
    const Compressed = CompressedState(fa);
    var cfa: [Compressed.accept_states]Compressed = undefined;

    @setEvalBranchQuota(1000000);
    for (fa, &cfa) |s, *cs| {
        // Convert all the transitions for this state
        for (0..256) |c| {
            // Transitions at this byte get converted to this class index
            const class = Compressed.classes[c];
            if (s.trans[c]) |trans| {
                // Just go to the transition
                cs.*.trans[class] = trans;
            } else {
                if (s.token) |tok| {
                    // Anything after the states array stops the dfa, this is the accept states
                    cs.*.trans[class] = Compressed.accept_states + tok;
                } else {
                    // Anything that is not an accept state is an error state
                    cs.*.trans[class] = Compressed.err_state;
                }
            }
        }
    }
    const final = cfa;
    return &final;
}

test "lexer" {
    const Lex = Lexer(void, .{
        .int = "int",
        .eq = "=",
        .semicolon = ";",
        .ident = struct {
            pub const capture = "[a-zA-Z_][a-zA-Z0-9_]*";
            pub fn action(_: void, matched: []const u8) []const u8 {
                return matched;
            }
        },
        .int_literal = struct {
            pub const capture = "0|([1-9][0-9]*)";
            pub fn action(_: void, matched: []const u8) !i64 {
                return std.fmt.parseInt(i64, matched, 10);
            }
        },
        .whitespace = struct {
            pub const skip = "[ \t\n]*";
        },
    });
    var l = Lex.init({}, "int a = 5;");

    try std.testing.expectEqualDeep(Lex.Token.int, try l.next());
    try std.testing.expectEqualDeep(Lex.Token{ .ident = "a" }, try l.next());
    try std.testing.expectEqualDeep(Lex.Token.eq, try l.next());
    try std.testing.expectEqualDeep(Lex.Token{ .int_literal = 5 }, try l.next());
    try std.testing.expectEqualDeep(Lex.Token.semicolon, try l.next());
}
