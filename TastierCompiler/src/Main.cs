using System;

namespace Tastier { // DMA

class Tastier { // DMA

  public static void Main (string[] arg) {
    if (arg.Length == 2) {
      Scanner scanner = new Scanner(arg[0]);
      Parser parser = new Parser(scanner);
      parser.Parse();
      if (parser.errors.count == 0) {
        parser.Write(arg[1]);
      }
    } else Console.WriteLine("Usage: tcc <program.TAS> <output.asm>");
  }

}

}
