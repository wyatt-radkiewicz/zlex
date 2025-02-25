const std = @import("std");
const zlex = @import("zlex");

// Kind of a JSON lexer?
const Lexer = zlex.Lexer(void, .{
    .open_brace = "{",
    .close_brace = "}",
    .whitespace = "[ \t\n\r]*",
    .string = "\"[ -~]*\"",
    .colon = ":",
    .comma = ",",
    .number = "[1-9][0-9]*",
    .true = "true",
    .false = "false",
    .null = "null",
});

pub fn main() !void {
    var file = try std.fs.cwd().openFile("examples/test.json", .{});
    defer file.close();
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const src = try file.readToEndAlloc(allocator, 1024 * 1024 * 16);
    defer allocator.free(src);
    const stdout_file = std.io.getStdOut();
    var buffered_writer = std.io.bufferedWriter(stdout_file.writer());
    defer buffered_writer.flush() catch @panic("can't flush");
    const stdout = buffered_writer.writer();
    
    try stdout.print("starting\n", .{});
    var lexer = Lexer.init({}, src);
    while (try lexer.next()) |tok| {
        try stdout.print("{s}\n", .{@tagName(tok)});
    }
}
