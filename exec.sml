(* exec.sml *)
CM.make("while.cm");
use "stack.sml";
use "vmc_machine.sml";
Control.Print.printLength := 10000; (* set printing parameters so that *)
Control.Print.printDepth := 10000; (* we’ll see all details *)
Control.Print.stringDepth := 10000; (* and strings *)