
using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

using Symbol = System.Tuple<string, int, int, int, int>;
using Instruction = System.Tuple<string,string>;
using StackAddress = System.Tuple<int, int>;

namespace Tastier {



public class Parser {
	public const int _EOF = 0;
	public const int _ident = 1;
	public const int _number = 2;
	public const int _str = 3;
	public const int maxT = 48;

	const bool T = true;
	const bool x = false;
	const int minErrDist = 2;

	public Scanner scanner;
	public Errors  errors;

	public Token t;    // last recognized token
	public Token la;   // lookahead token
	int errDist = minErrDist;

enum TastierType : int {   // types for variables
    Undefined,
    Integer,
    Boolean,
    String,
    Char
  };

  enum TastierKind : int {  // kinds of symbol
    Const,
    Var,
    Proc,
    Struct
  };

/*
  You'll notice some type aliases, such as the one just below, are commented
  out. This is because C# only allows using-alias-directives outside of a
  class, while class-inheritance directives are allowed inside. So the
  snippet immediately below is illegal in here. To complicate matters
  further, the C# runtime does not properly handle class-inheritance
  directives for Tuples (it forces you to write some useless methods). For
  these reasons, the type aliases which alias Tuples can be found in
  Parser.frame, but they're documented in this file, with the rest.
*/

  //using Symbol = System.Tuple<string, int, int, int, int>;

/*
  A Symbol is a name with a type and a kind. The first int in the
  tuple is the kind, and the second int is the type. We'll use these to
  represent declared names in the program.

  For each Symbol which is a variable, we have to allocate some storage, so
  the variable lives at some address in memory. The address of a variable on
  the stack at runtime has two components. The first component is which
  stack frame it's in, relative to the current procedure. If the variable is
  declared in the procedure that's currently executing, then it will be in
  that procedure's stack frame. If it's declared in the procedure that
  called the currently active one, then it'll be in the caller's stack
  frame, and so on. The first component is the offset that says how many
  frames up the chain of procedure calls to look for the variable. The
  second component is simply the location of the variable in the stack frame
  where it lives.

  The third int in the symbol is the stack frame on which the variable
  lives, and the fourth int is the index in that stack frame. Since
  variables which are declared in the global scope aren't inside any
  function, they don't have a stack frame to go into. In this compiler, our
  convention is to put these variables at an address in the data memory. If
  the variable was declared in the global scope, the fourth field in the
  Symbol will be zero, and we know that the next field is an address in
  global memory, not on the stack.

  Procedures, on the other hand, are just sets of instructions. A procedure
  is not data, so it isn't stored on the stack or in memory, but is just a
  particular part of the list of instructions in the program being run. If
  the symbol is the name of a procedure, we'll store a -1 in the address
  field (5).

  When the program is being run, the code will be loaded into the machine's
  instruction memory, and the procedure will have an address there. However,
  it's easier for us to just give the procedure a unique label, instead of
  remembering what address it lives at. The assembler will take care of
  converting the label into an address when it encounters a JMP, FJMP or
  CALL instruction with that label as a target.

  To summarize:
    * Symbol.Item1 -> name
    * Symbol.Item2 -> kind
    * Symbol.Item3 -> type
    * Symbol.Item4 -> stack frame pointer
    * Symbol.Item5 -> variable's address in the stack frame pointed to by
                      Item4, -1 if procedure
*/

  class Scope : Stack<Symbol> {}

/*
  A scope contains a stack of symbol definitions. Every time we come across
  a new local variable declaration, we can just push it onto the stack. We'll
  use the position of the variable in the stack to represent its address in
  the stack frame of the procedure in which it is defined. In other words, the
  variable at the bottom of the stack goes at location 0 in the stack frame,
  the next variable at location 1, and so on.
*/

  //using Instruction = Tuple<string, string>;
  class Program : List<Instruction> {}

/*
  A program is just a list of instructions. When the program is loaded into
  the machine's instruction memory, the instructions will be laid out in the
  same order that they appear in this list. Because of this, we can use the
  location of an instruction in the list as its address in instruction memory.
  Labels are just names for particular locations in the list of instructions
  that make up the program.

  The first component of all instructions is a label, which can be empty.
  The second component is the actual instruction itself.

  To summarize:
    * Instruction.Item1 -> label
    * Instruction.Item2 -> the actual instruction, as a string
*/

Stack<Scope> openScopes = new Stack<Scope>();
Scope externalDeclarations = new Scope();

/*
  Every time we encounter a new procedure declaration in the program, we want
  to make sure that expressions inside the procedure see all of the variables
  that were in scope at the point where the procedure was defined. We also
  want to make sure that expressions outside the procedure do not see the
  procedure's local variables. Every time we encounter a procedure, we'll push
  a new scope on the stack of open scopes. When the procedure ends, we can pop
  it off and continue, knowing that the local variables defined in the
  procedure cannot be seen outside, since we've popped the scope which
  contains them off the stack.
*/

Program program = new Program();
Program header = new Program();

Stack<string> openProcedureDeclarations = new Stack<string>();

/*
  In order to implement the "shadowing" of global procedures by local procedures
  properly, we need to generate a label for local procedures that is different
  from the label given to procedures of the same name in outer scopes. See the
  test case program "procedure-label-shadowing.TAS" for an example of why this
  is important. In order to make labels unique, when we encounter a non-global
  procedure declaration called "foo" (for example), we'll give it the label
  "enclosingProcedureName$foo" for all enclosing procedures. So if it's at
  nesting level 2, it'll get the label "outermost$nextoutermost$foo". Let's
  make a function that does this label generation given the set of open
  procedures which enclose some new procedure name.
*/

string generateProcedureName(string name) {
  if (openProcedureDeclarations.Count == 0) {
    return name;
  } else {
    string temp = name;
    foreach (string s in openProcedureDeclarations) {
      temp = s + "$" + temp;
    }
    return temp;
  }
}

/*
  We also need a function that figures out, when we call a procedure from some
  scope, what label to call. This is where we actually implement the shadowing;
  the innermost procedure with that name should be called, so we have to figure
  out what the label for that procedure is.
*/

string getLabelForProcedureName(int lexicalLevelDifference, string name) {
  /*
     We want to skip <lexicalLevelDifference> labels backwards, but compose
     a label that incorporates the names of all the enclosing procedures up
     to that point. A lexical level difference of zero indicates a procedure
     defined in the current scope; a difference of 1 indicates a procedure
     defined in the enclosing scope, and so on.
  */
  int numOpenProcedures = openProcedureDeclarations.Count;
  int numNamesToUse = (numOpenProcedures - lexicalLevelDifference);
  string theLabel = name;

  /*
    We need to concatenate the first <numNamesToUse> labels with a "$" to
    get the name of the label we need to call.
  */

  var names = openProcedureDeclarations.Take(numNamesToUse);

  foreach (string s in names) {
      theLabel = s + "$" + theLabel;
  }

  return theLabel;
}

Stack<string> openLabels = new Stack<string>();
int labelSeed = 0;

string generateLabel() {
  return "L$"+labelSeed++;
}

/*
  Sometimes, we need to jump over a block of code which we're about to
  generate (for example, at the start of a loop, if the test fails, we have
  to jump to the end of the loop). Because it hasn't been generated yet, we
  don't know how long it will be (in the case of the loop, we don't know how
  many instructions will be in the loop body until we actually generate the
  code, and count them). In this case, we can make up a new label for "the
  end of the loop" and emit a jump to that label. When we get to the end of
  the loop, we can put the label in, so that the jump will go to the
  labelled location. Since we can have loops within loops, we need to keep
  track of which label is the one that we are currently trying to jump to,
  and we need to make sure they go in the right order. We'll use a stack to
  store the labels for all of the forward jumps which are active. Every time
  we need to do a forward jump, we'll generate a label, emit a jump to that
  label, and push it on the stack. When we get to the end of the loop, we'll
  put the label in, and pop it off the stack.
*/

Symbol _lookup(Scope scope, string name) {
  foreach (Symbol s in scope) {
      if (s.Item1 == name) {
        return s;
      }
  }
  return null;
}

Symbol lookup(Stack<Scope> scopes, string name) {
  int stackFrameOffset = 0;
  int variableOffset = 0;

  foreach (Scope scope in scopes) {
    foreach (Symbol s in scope) {
      if (s.Item1 == name) {
        return s;
      }
      else {
        variableOffset += 1;
      }
    }
    stackFrameOffset += 1;
    variableOffset = 0;
  }
  return null; // if the name wasn't found in any open scopes.
}


Symbol[] lookupStruct(Stack<Scope> scopes, string name) {
  int stackFrameOffset = 0;
  int variableOffset = 0;
  List<Symbol> resSyms = new List<Symbol>();

  foreach (Scope scope in scopes) {
    foreach (Symbol s in scope) {
      if(s.Item1.Contains(".")){
        string[] sep = {"."};
        string[] res = s.Item1.Split(sep, StringSplitOptions.RemoveEmptyEntries);
        
        if (res.Length==2 && res[0]==name) {
          resSyms.Add(s);
        }
      }
      else {
        variableOffset += 1;
      }
    }
    if(resSyms.Count != 0){
      return resSyms.ToArray();
    }
    stackFrameOffset += 1;
    variableOffset = 0;
  }
  return null; // if the name wasn't found in any open scopes.
}

/*
  You may notice that when we use a LoadG or StoG instruction, we add 3 to
  the address of the item being loaded or stored. This is because the
  control and status registers of the machine are mapped in at addresses 0,
  1, and 2 in data memory, so we cannot use those locations for storing
  variables. If you want to load rtp, rbp, or rpc onto the stack to
  manipulate them, you can LoadG and StoG to those locations.
*/


/*the below stack allows for nesting break statements inside nested loops*/
Stack<string> openBreakableStats = new Stack<string>();

/*--------------------------------------------------------------------------*/



	public Parser(Scanner scanner) {
		this.scanner = scanner;
		errors = new Errors();
	}

	void SynErr (int n) {
		if (errDist >= minErrDist) errors.SynErr(la.line, la.col, n);
		errDist = 0;
	}

	public void SemErr (string msg) {
		if (errDist >= minErrDist) errors.SemErr(t.line, t.col, msg);
		errDist = 0;
	}

  public void Warn (string msg) {
    Console.WriteLine("-- line " + t.line + " col " + t.col + ": " + msg);
  }

  public void Write (string filename) {
    List<string> output = new List<string>();
    foreach (Instruction i in header) {
      if (i.Item1 != "") {
        output.Add(i.Item1 + ": " + i.Item2);
      } else {
        output.Add(i.Item2);
      }
    }
    File.WriteAllLines(filename, output.ToArray());
  }

	void Get () {
		for (;;) {
			t = la;
			la = scanner.Scan();
			if (la.kind <= maxT) { ++errDist; break; }

			la = t;
		}
	}

	void Expect (int n) {
		if (la.kind==n) Get(); else { SynErr(n); }
	}

	bool StartOf (int s) {
		return set[s, la.kind];
	}

	void ExpectWeak (int n, int follow) {
		if (la.kind == n) Get();
		else {
			SynErr(n);
			while (!StartOf(follow)) Get();
		}
	}


	bool WeakSeparator(int n, int syFol, int repFol) {
		int kind = la.kind;
		if (kind == n) {Get(); return true;}
		else if (StartOf(repFol)) {return false;}
		else {
			SynErr(n);
			while (!(set[syFol, kind] || set[repFol, kind] || set[0, kind])) {
				Get();
				kind = la.kind;
			}
			return StartOf(syFol);
		}
	}


	void AddOp(out Instruction inst) {
		inst = new Instruction("", "Add"); 
		if (la.kind == 4) {
			Get();
		} else if (la.kind == 5) {
			Get();
			inst = new Instruction("", "Sub"); 
		} else SynErr(49);
	}

	void Expr(out TastierType type) {
		TastierType type1; Instruction inst; 
		type = TastierType.Undefined;
		SimExpr(out type);
		if (StartOf(1)) {
			RelOp(out inst);
			SimExpr(out type1);
			if (type != type1) {
			 SemErr("incompatible types");
			}
			else {
			 program.Add(inst);
			 type = TastierType.Boolean;
			}
			
		}
	}

	void SimExpr(out TastierType type) {
		TastierType type1; Instruction inst; 
		Term(out type);
		while (la.kind == 4 || la.kind == 5) {
			AddOp(out inst);
			Term(out type1);
			if (type != TastierType.Integer || type1 != TastierType.Integer) {
			 SemErr("integer type expected");
			}
			program.Add(inst);
			
		}
	}

	void RelOp(out Instruction inst) {
		inst = new Instruction("", "Equ"); 
		switch (la.kind) {
		case 18: {
			Get();
			break;
		}
		case 19: {
			Get();
			inst = new Instruction("", "NEq"); 
			break;
		}
		case 20: {
			Get();
			inst = new Instruction("", "Lss"); 
			break;
		}
		case 21: {
			Get();
			inst = new Instruction("", "Gtr"); 
			break;
		}
		case 22: {
			Get();
			inst = new Instruction("", "LEq"); 
			break;
		}
		case 23: {
			Get();
			inst = new Instruction("", "GEq"); 
			break;
		}
		default: SynErr(50); break;
		}
	}

	void Factor(out TastierType type) {
		int n; Symbol sym; string name; 
		type = TastierType.Undefined; 
		if (la.kind == 1) {
			Ident(out name);
			if (la.kind == 6) {
				Get();
				string structName = name;
				Ident(out name);
				name = structName + "." + name;
			}
			while (la.kind == 7) {
				Get();
				Expect(2);
				name += "["+t.val+"]"; 
				Expect(8);
			}
			bool isExternal = false; //CS3071 students can ignore external declarations, since they only deal with compilation of single files.
			sym = lookup(openScopes, name);
			if (sym == null) {
			 sym = _lookup(externalDeclarations, name);
			 isExternal = true;
			}
			
			if (sym == null) {
			 SemErr("reference to undefined variable " + name);
			}
			else {
			 type = (TastierType)sym.Item3;
			 if ((TastierKind)sym.Item2 == TastierKind.Var ) {
			   if (sym.Item4 == 0) {
			       if (isExternal) {
			         program.Add(new Instruction("", "LoadG " + sym.Item1));
			         // if the symbol is external, we load it by name. The linker will resolve the name to an address.
			       } else {
			         program.Add(new Instruction("", "LoadG " + (sym.Item5+3)));
			       }
			   } else {
			       int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
			       program.Add(new Instruction("", "Load " + lexicalLevelDifference + " " + sym.Item5));
			   }
			 } else if ((TastierKind)sym.Item2 == TastierKind.Const) {
			   program.Add(new Instruction("", "Const " + sym.Item5));
			 } else SemErr("Variable or a const expected");
			}
			
		} else if (la.kind == 2) {
			Get();
			n = Convert.ToInt32(t.val);
			program.Add(new Instruction("", "Const " + n));
			type = TastierType.Integer;
			
		} else if (la.kind == 5) {
			Get();
			Factor(out type);
			if (type != TastierType.Integer) {
			 SemErr("integer type expected");
			 type = TastierType.Integer;
			}
			program.Add(new Instruction("", "Neg"));
			program.Add(new Instruction("", "Const 1"));
			program.Add(new Instruction("", "Add"));
			
		} else if (la.kind == 9) {
			Get();
			program.Add(new Instruction("", "Const " + 1)); type = TastierType.Boolean; 
		} else if (la.kind == 10) {
			Get();
			program.Add(new Instruction("", "Const " + 0)); type = TastierType.Boolean; 
		} else SynErr(51);
	}

	void Ident(out string name) {
		Expect(1);
		name = t.val; 
	}

	void MulOp(out Instruction inst) {
		inst = new Instruction("", "Mul"); 
		if (la.kind == 11) {
			Get();
		} else if (la.kind == 12) {
			Get();
			inst = new Instruction("", "Div"); 
		} else SynErr(52);
	}

	void ProcDecl() {
		string name; string label; Scope currentScope = openScopes.Peek(); int enterInstLocation = 0; bool external = false; 
		Expect(13);
		Ident(out name);
		currentScope.Push(new Symbol(name, (int)TastierKind.Proc, (int)TastierType.Undefined, openScopes.Count, -1));
		openScopes.Push(new Scope());
		currentScope = openScopes.Peek();
		
		Expect(14);
		Expect(15);
		Expect(16);
		program.Add(new Instruction("", "Enter 0"));
		enterInstLocation = program.Count - 1;
		label = generateProcedureName(name);
		openProcedureDeclarations.Push(name);
		/*
		 Enter is supposed to have as an
		 argument the next free address on the
		 stack, but until we know how many
		 local variables are in this procedure,
		 we don't know what that is. We'll keep
		 track of where we put the Enter
		 instruction in the program so that
		 later, when we know how many spaces on
		 the stack have been allocated, we can
		 put the right value in.
		*/
		
		while (StartOf(2)) {
			if (StartOf(3)) {
				VarDecl(external);
			} else if (StartOf(4)) {
				Stat();
			} else {
				openLabels.Push(generateLabel());
				program.Add(new Instruction("", "Jmp " + openLabels.Peek()));
				/*
				 We need to jump over procedure
				 definitions because otherwise we'll
				 execute all the code inside them!
				 Procedures should only be entered via
				 a Call instruction.
				*/
				
				ProcDecl();
				program.Add(new Instruction(openLabels.Pop(), "Nop")); 
			}
		}
		Expect(17);
		program.Add(new Instruction("", "Leave"));
		program.Add(new Instruction("", "Ret"));
		openScopes.Pop();
		// now we can generate the Enter instruction properly
		program[enterInstLocation] =
		 new Instruction(label, "Enter " +
		                 currentScope.Count(s => s.Item2 == (int)TastierKind.Var));
		openProcedureDeclarations.Pop();
		
	}

	void VarDecl(bool external) {
		Symbol sym; string name; TastierType type; Scope currentScope = openScopes.Peek(); List<int> dims = new List<int>(); bool arr = false;
		
		Type(out type);
		Ident(out name);
		if (external) {
		 sym = new Symbol(name, (int)TastierKind.Var, (int)type, 0, 0);
		 externalDeclarations.Push(sym);
		} else {
		 sym = new Symbol(name, (int)TastierKind.Var, (int)type, openScopes.Count-1, currentScope.Count( s => s.Item2 == (int)TastierKind.Var ));
		 currentScope.Push(sym);
		}
		
		while (la.kind == 7) {
			Get();
			Expect(2);
			int dimLength = Convert.ToInt32(t.val);
			dims.Add(dimLength);
			program.Add(new Instruction("","Const " + dimLength));
			
			if (sym.Item4 == 0) {
			 program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
			}
			else {
			 int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
			 program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
			}
			
			arr = true;
			
			Expect(8);
		}
		if(arr){
		 /*************************ALLOCATE SPACE FOR THE ELEMENTS*************************/
		 int totLen = 1;
		 for(int i = 0; i < dims.Count; i++)
		   totLen *= dims[i];
		 
		 int[] tArr = dims.ToArray();
		 
		 for(int i = 0; tArr.Length > i; i++)
		   tArr[i] = 0;
		   
		 int count = 0;
		 bool finished = false;
		 while(!finished){
		   count++;
		   string tempName = name;
		   for(int i = 0; i < tArr.Length; i++)
		     tempName += "[" + tArr[i] + "]";
		   
		   Symbol elemSym;
		   if (external) {
		     elemSym = new Symbol(tempName, (int)TastierKind.Var, (int)type, 0, 0);
		     externalDeclarations.Push(elemSym);
		   } else {
		     elemSym = new Symbol(tempName, (int)TastierKind.Var, (int)type, openScopes.Count-1, currentScope.Count( s => s.Item2 == (int)TastierKind.Var ));
		     currentScope.Push(elemSym);
		   }
		   
		   if (elemSym.Item4 == 0) {
		     program.Add(new Instruction("", "StoG " + (elemSym.Item5+3)));
		   }
		   else {
		     int lexicalLevelDifference = Math.Abs(openScopes.Count - elemSym.Item4)-1;
		     program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + elemSym.Item5));
		   }
		   
		   tArr[tArr.Length-1]++;
		   tArr[tArr.Length-1] = tArr[tArr.Length-1] % dims[tArr.Length-1];
		   for(int c = (tArr.Length-1); tArr[c]==0;){
		     tArr[c] = 0;
		     c--;
		     if(c < 0){
		       finished = true;
		       break;
		     }
		     tArr[c]++;
		     tArr[c] = tArr[c]%dims[c];
		   }
		 }
		}
		
		while (la.kind == 45) {
			Get();
			Ident(out name);
			if (external) {
			 externalDeclarations.Push(new Symbol(name, (int)TastierKind.Var, (int)type, 0, 0));
			} else {
			 currentScope.Push(new Symbol(name, (int)TastierKind.Var, (int)type, openScopes.Count-1, currentScope.Count(s => s.Item2 == (int)TastierKind.Var)));
			}
			
		}
		Expect(25);
	}

	void Stat() {
		TastierType type; string name; Symbol sym; bool external = false; bool isExternal = false; 
		switch (la.kind) {
		case 1: {
			Ident(out name);
			if (la.kind == 6) {
				Get();
				string structName = name;
				Ident(out name);
				name = structName +"."+ name;
			}
			while (la.kind == 7) {
				Get();
				Expect(2);
				name += "["+t.val+"]"; 
				
				Expect(8);
			}
			sym = lookup(openScopes, name);
			if (sym == null) {
			 sym = _lookup(externalDeclarations, name);
			 isExternal = true;
			}
			if (sym == null) {
			 SemErr("reference to undefined variable " + name);
			}
			
			if (la.kind == 24) {
				Get();
				if ((TastierKind)sym.Item2 != TastierKind.Var) {
				 SemErr("cannot assign to non-variable");
				}
				
				Expr(out type);
				TastierType resType = type;
				if (la.kind == 25) {
					Get();
					if (resType != (TastierType)sym.Item3) {
					 SemErr("incompatible types");
					}
					
					if (sym.Item4 == 0) {
					 if (isExternal) {
					   program.Add(new Instruction("", "StoG " + sym.Item1));
					   // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
					 } else {
					   program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
					 }
					}
					else {
					 int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
					 program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
					}
					
				} else if (la.kind == 26) {
					if (resType != TastierType.Boolean){
					 SemErr("expected boolean type in the conditional statement: Ident=<Expr>?Expr:Expr");
					}
					openLabels.Push(generateLabel());	//pushes label onto the stack of labels
					program.Add(new Instruction("", "FJmp " + openLabels.Peek())); //jump to the label from top of the stack if last value false or 0
					
					Get();
					Expr(out type);
					Expect(27);
					if (type != (TastierType)sym.Item3) {
					   SemErr("incompatible return type in the conditional statement: Ident=Expr?Expr:<Expr>");
					}
					
					/************************/
					Instruction startOfElse = new
					Instruction(openLabels.Pop(), "Nop");//instr that will set the label in the assembly file
					/*
					  If we got into the "if", we need to
					  jump over the "else" so that it
					  doesn't get executed.
					*/
					openLabels.Push(generateLabel());  //create new label and put it on top of the stack
					program.Add(new Instruction("", "Jmp " + openLabels.Peek())); //jump to the newly generated label that will define the end of the  else statement 
					program.Add(startOfElse); //put the label of  start of else here
					/************************/
					
					if (sym.Item4 == 0) {
					 if (isExternal) {
					   program.Add(new Instruction("", "StoG " + sym.Item1));
					   // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
					 } else {
					   program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
					 }
					}
					else {
					 int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
					 program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
					}
					 
					
					Expr(out type);
					Expect(25);
					if (type != (TastierType)sym.Item3) {
					   SemErr("incompatible types in Ident=Expr?Expr:<Expr>");
					}
					
					/************************/
					program.Add(new Instruction(openLabels.Pop(), "Nop"));
					/************************/
					
					if (sym.Item4 == 0) {
					 if (isExternal) {
					   program.Add(new Instruction("", "StoG " + sym.Item1));
					   // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
					 } else {
					   program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
					 }
					}
					else {
					 int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
					 program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
					}
					
				} else SynErr(53);
			} else if (la.kind == 14) {
				Get();
				Expect(15);
				Expect(25);
				if ((TastierKind)sym.Item2 != TastierKind.Proc) {
				 SemErr("object is not a procedure");
				}
				
				int currentStackLevel = openScopes.Count;
				int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4);
				string procedureLabel = getLabelForProcedureName(lexicalLevelDifference, sym.Item1);
				program.Add(new Instruction("", "Call " + lexicalLevelDifference + " " + procedureLabel));
				
			} else SynErr(54);
			break;
		}
		case 28: {
			Get();
			Ident(out name);
			string structName = name;
			Scope currentScope = openScopes.Peek();
			
			Ident(out name);
			Expect(25);
			sym = lookup(openScopes, structName);
			if(sym.Item2 != (int)TastierKind.Struct){
			 SemErr(structName + " is not an already exisiting structure. ");
			}
			Symbol[] structItems = lookupStruct(openScopes, structName);
			int len = structItems.Length;
			for(int i = 0; i < len; i++){
			 
			 string[] sep = {"."};
			 string varName = (structItems[i].Item1.Split(sep, StringSplitOptions.RemoveEmptyEntries))[1];
			 Console.WriteLine(varName);
			 currentScope.Push(new Symbol(name+"."+varName, (int)TastierKind.Var, (int)TastierType.Integer, 
			   openScopes.Count-1, currentScope.Count(s => s.Item2 == (int)TastierKind.Var)));
			}
			
			
			break;
		}
		case 29: {
			Get();
			Symbol switchSym; int n;
			Expect(14);
			Ident(out name);
			Expect(15);
			openBreakableStats.Push(generateLabel());
			switchSym = lookup(openScopes, name);
			if(switchSym.Item2 != (int)TastierKind.Var){
			 SemErr("only variables may be switched.");
			}else if(switchSym.Item3 != (int)TastierType.Integer){
			 SemErr("only variables of type Integer can be switched");
			}
			
			Expect(16);
			while (la.kind == 30) {
				Get();
				Expect(2);
				openLabels.Push(generateLabel());
				n = Convert.ToInt32(t.val);
				program.Add(new Instruction("", "Const " + n));
				if (switchSym.Item4 == 0) {
				 if (isExternal) {
				   program.Add(new Instruction("", "LoadG " + switchSym.Item1));
				   // if the symbol is external, we load it by name. The linker will resolve the name to an address.
				 } else {
				   program.Add(new Instruction("", "LoadG " + (switchSym.Item5+3)));
				 }
				} else {
				 int lexicalLevelDifference = Math.Abs(openScopes.Count - switchSym.Item4)-1;
				 program.Add(new Instruction("", "Load " + lexicalLevelDifference + " " + switchSym.Item5));
				}
				program.Add(new Instruction("", "Equ"));
				program.Add(new Instruction("", "FJmp " + openLabels.Peek()));
				
				if (StartOf(4)) {
					Stat();
				}
				program.Add(new Instruction(openLabels.Pop(), "Nop"));
			}
			if (la.kind == 31) {
				Get();
				if (StartOf(4)) {
					Stat();
				}
			}
			Expect(17);
			program.Add(new Instruction(openBreakableStats.Pop(), "Nop"));
			break;
		}
		case 32: {
			Get();
			Expect(25);
			program.Add(new Instruction("", "Jmp " + openBreakableStats.Peek()));
			
			break;
		}
		case 33: {
			Get();
			Expect(14);
			Expr(out type);
			Expect(15);
			if ((TastierType)type != TastierType.Boolean) {
			 SemErr("boolean type expected");
			}
			openLabels.Push(generateLabel());
			program.Add(new Instruction("", "FJmp " + openLabels.Peek()));
			
			Stat();
			Instruction startOfElse = new Instruction(openLabels.Pop(), "Nop");
			/*
			  If we got into the "if", we need to
			  jump over the "else" so that it
			  doesn't get executed.
			*/
			openLabels.Push(generateLabel());
			program.Add(new Instruction("", "Jmp " + openLabels.Peek()));
			program.Add(startOfElse);
			
			if (la.kind == 34) {
				Get();
				Stat();
			}
			program.Add(new Instruction(openLabels.Pop(), "Nop")); 
			break;
		}
		case 35: {
			Get();
			string loopStartLabel = generateLabel();
			openLabels.Push(generateLabel()); //second label is for the loop end
			program.Add(new Instruction(loopStartLabel, "Nop"));
			
			Expect(14);
			Expr(out type);
			Expect(15);
			if ((TastierType)type != TastierType.Boolean) {
			 SemErr("boolean type expected");
			}
			program.Add(new Instruction("", "FJmp " + openLabels.Peek())); // jump to the loop end label if condition is false
			
			Stat();
			program.Add(new Instruction("", "Jmp " + loopStartLabel));
			program.Add(new Instruction(openLabels.Pop(), "Nop")); // put the loop end label here
			
			break;
		}
		case 36: {
			Get();
			Instruction updateActionInstr;
			
			Expect(14);
			Ident(out name);
			sym = lookup(openScopes, name);
			if(sym == null){
			   SemErr("reference to undefined variable " + name);
			}
			
			if (la.kind == 24) {
				Get();
				Expr(out type);
				if((int)type != sym.Item2){
				   SemErr("variable type expected but " + sym.Item1 + " has kind " + (TastierType)sym.Item2);
				}
				if (sym.Item4 == 0) {
				   if (isExternal) {
				       program.Add(new Instruction("", "StoG " + sym.Item1));
				       // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
				   } else {
				       program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
				   }
				}
				else {
				   int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
				   program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
				}
				
			}
			Expect(25);
			openLabels.Push(generateLabel());
			Instruction goBackToTheTop = new Instruction("", "Jmp " + openLabels.Peek());
			program.Add(new Instruction(openLabels.Pop(), "Nop"));
			
			Ident(out name);
			Expect(24);
			sym = lookup(openScopes, name);
			if(sym == null){
			   SemErr("reference to undefined variable " + name);
			}
			
			Expr(out type);
			Expect(25);
			if((int)type != sym.Item2){
			   SemErr(type + " variable type expected but " + sym.Item1 + " has kind " + (TastierType)sym.Item2);
			}
			
			if (sym.Item4 == 0) {
			   if (isExternal) {
			       updateActionInstr = new Instruction("", "StoG " + sym.Item1);
			       // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
			   } else {
			       updateActionInstr = new Instruction("", "StoG " + (sym.Item5+3));
			   }
			}
			else {
			   int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
			   updateActionInstr = new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5);
			}
			
			Expr(out type);
			Expect(15);
			if(type != TastierType.Boolean){
			   SemErr("Boolean variable type expected but has kind " + type);
			}
			
			openLabels.Push(generateLabel());
			program.Add(new Instruction("","FJmp " + openLabels.Peek()));
			
			Stat();
			program.Add(updateActionInstr);
			program.Add(goBackToTheTop);
			program.Add(new Instruction(openLabels.Pop(),"Nop"));
			
			break;
		}
		case 37: {
			Get();
			Ident(out name);
			Expect(25);
			sym = lookup(openScopes, name);
			if (sym == null) {
			 sym = _lookup(externalDeclarations, name);
			 isExternal = true;
			}
			if (sym == null) {
			 SemErr("reference to undefined variable " + name);
			}
			
			if (sym.Item2 != (int)TastierKind.Var) {
			 SemErr("variable type expected but " + sym.Item1 + " has kind " + (TastierType)sym.Item2);
			}
			
			if (sym.Item3 != (int)TastierType.Integer) {
			 SemErr("integer type expected but " + sym.Item1 + " has type " + (TastierType)sym.Item2);
			}
			program.Add(new Instruction("", "Read"));
			
			if (sym.Item4 == 0) {
			 if (isExternal) {
			   program.Add(new Instruction("", "StoG " + sym.Item1));
			   // if the symbol is external, we also store it by name. The linker will resolve the name to an address.
			 } else {
			   program.Add(new Instruction("", "StoG " + (sym.Item5+3)));
			 }
			}
			else {
			 int lexicalLevelDifference = Math.Abs(openScopes.Count - sym.Item4)-1;
			 program.Add(new Instruction("", "Sto " + lexicalLevelDifference + " " + sym.Item5));
			}
			
			break;
		}
		case 38: {
			Get();
			Expr(out type);
			if (type == TastierType.Integer) {
			 program.Add(new Instruction("", "Write"));
			}
			else if(type == TastierType.String) {
			 string varName = t.val;
			 sym = lookup(openScopes, varName);
			 int len = sym.Item5;
			 for(int i = 0; i < len; i++){
			   sym = lookup(openScopes, varName+"["+i+"]");
			   program.Add(new Instruction("", "Const " + sym.Item5));
			   program.Add(new Instruction("", "WriteChar"));
			 }
			} 
			else {
			 SemErr("integer or string type expected");
			}
			
			Expect(25);
			break;
		}
		case 16: {
			Get();
			while (StartOf(5)) {
				if (StartOf(4)) {
					Stat();
				} else {
					VarDecl(external);
				}
			}
			Expect(17);
			break;
		}
		default: SynErr(55); break;
		}
	}

	void Term(out TastierType type) {
		TastierType type1; Instruction inst; 
		Factor(out type);
		while (la.kind == 11 || la.kind == 12) {
			MulOp(out inst);
			Factor(out type1);
			if (type != TastierType.Integer ||
			   type1 != TastierType.Integer) {
			 SemErr("integer type expected");
			}
			program.Add(inst);
			
		}
	}

	void Tastier() {
		string name; bool external = false; 
		Expect(39);
		Ident(out name);
		openScopes.Push(new Scope());
		
		Expect(16);
		while (la.kind == 28 || la.kind == 44) {
			if (la.kind == 44) {
				ConstDecl();
			} else {
				StructDecl();
			}
		}
		while (StartOf(6)) {
			if (StartOf(3)) {
				VarDecl(external);
			} else if (la.kind == 13) {
				ProcDecl();
			} else {
				ExternDecl();
			}
		}
		Expect(17);
		if (openScopes.Peek().Count == 0) {
		 Warn("Warning: Program " + name + " is empty ");
		}
		
		header.Add(new Instruction("", ".names " + (externalDeclarations.Count + openScopes.Peek().Count)));
		foreach (Symbol s in openScopes.Peek()) {
		 if (s.Item2 == (int)TastierKind.Var) {
		   header.Add(new Instruction("", ".var " + ((int)s.Item3) + " " + s.Item1));
		 } else if (s.Item2 == (int)TastierKind.Proc) {
		   header.Add(new Instruction("", ".proc " + s.Item1));
		 } else if (s.Item2 != (int)TastierKind.Const && s.Item2 != (int)TastierKind.Struct){
		   Console.WriteLine("was here : " + s.Item1);
		   SemErr("global item " + s.Item1 + " has no defined type");
		 } 
		}
		
		foreach (Symbol s in externalDeclarations) {
		 if (s.Item2 == (int)TastierKind.Var) {
		   header.Add(new Instruction("", ".external var " + ((int)s.Item3) + " " + s.Item1));
		 } else if (s.Item2 == (int)TastierKind.Proc) {
		   header.Add(new Instruction("", ".external proc " + s.Item1));
		 } else {
		   SemErr("external item " + s.Item1 + " has no defined type" + s.Item3);
		 }
		}
		header.AddRange(program);
		openScopes.Pop();
		
	}

	void ConstDecl() {
		string name; int type = 0; int val = 0; Scope currentScope = openScopes.Peek();
		
		Expect(44);
		Ident(out name);
		if(_lookup(currentScope, name) != null){
		 SemErr("redefined identifier in the same scope.");
		}
		
		Expect(24);
		if (la.kind == 2) {
			Get();
			type = (int)TastierType.Integer; val = Convert.ToInt32(t.val); currentScope.Push(new Symbol(name, (int)TastierKind.Const, type, openScopes.Count-1, val));
		} else if (la.kind == 9 || la.kind == 10) {
			if (la.kind == 9) {
				Get();
				type = (int)TastierType.Boolean; val = 1; currentScope.Push(new Symbol(name, (int)TastierKind.Const, type, openScopes.Count-1, val));
			} else {
				Get();
			}
			type = (int)TastierType.Boolean; val = 0; currentScope.Push(new Symbol(name, (int)TastierKind.Const, type, openScopes.Count-1, val));
		} else if (la.kind == 3) {
			Get();
			type = (int)TastierType.String;
			currentScope.Push(new Symbol(name, (int)TastierKind.Const, (int)TastierType.String, openScopes.Count-1, t.val.Length-2));
			/*
			 putting each character temporarly on the stack as a character type
			 each character gets a name of <name>[index]
			*/
			for(int i = 1; i < t.val.Length-1; i++){
			 currentScope.Push(new Symbol(name+"["+(i-1)+"]", (int)TastierKind.Const, (int)TastierType.Char, openScopes.Count-1, (int)t.val[i]));
			}
			
		} else SynErr(56);
		Expect(25);
	}

	void StructDecl() {
		string structName; Symbol structSym; Scope currentScope = openScopes.Peek();
		Expect(28);
		Ident(out structName);
		structSym = new Symbol(structName, (int)TastierKind.Struct, (int)TastierType.Undefined, openScopes.Count-1, currentScope.Count(s => s.Item2 == (int)TastierKind.Var));
		currentScope.Push(structSym);
		
		
		Expect(16);
		while (StartOf(3)) {
			string name; TastierType type;
			Type(out type);
			if(type == TastierType.String){
			 SemErr("integer or boolean type variable declaration expected instead received " + type);
			}
			
			Ident(out name);
			if(_lookup(currentScope, structName+"."+name) != null){
			 SemErr("the variable " + name + "already exists in the " + structName + " structure.");
			}
			
			currentScope.Push(new Symbol(structName + "." + name, (int)TastierKind.Var, (int)type, openScopes.Count-1, currentScope.Count(s => s.Item2 == (int)TastierKind.Var)));
			
			Expect(25);
		}
		Expect(17);
	}

	void ExternDecl() {
		string name; bool external = true; Scope currentScope = openScopes.Peek(); int count = currentScope.Count; 
		Expect(46);
		if (StartOf(3)) {
			VarDecl(external);
		} else if (la.kind == 47) {
			Get();
			Ident(out name);
			Expect(25);
			externalDeclarations.Push(new Symbol(name, (int)TastierKind.Proc, (int)TastierType.Integer, 1, -1)); 
		} else SynErr(57);
	}

	void Type(out TastierType type) {
		type = TastierType.Undefined; 
		if (la.kind == 40) {
			Get();
			type = TastierType.Integer; 
		} else if (la.kind == 41) {
			Get();
			type = TastierType.Boolean; 
		} else if (la.kind == 42) {
			Get();
			type = TastierType.String; 
		} else if (la.kind == 43) {
			Get();
			type = TastierType.Char; 
		} else SynErr(58);
	}



	public void Parse() {
		la = new Token();
		la.val = "";
		Get();
		Tastier();
		Expect(0);

	}

	static readonly bool[,] set = {
		{T,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,T,T, T,T,T,T, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,x,x, x,x,x,x, x,x,x,x, x,T,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, T,T,x,T, T,T,T,x, T,T,T,T, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, x,x,x,x, x,x},
		{x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, T,T,x,T, T,T,T,x, x,x,x,x, x,x,x,x, x,x},
		{x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,x,x,x, x,x,x,x, x,x,x,x, T,T,x,x, T,T,x,T, T,T,T,x, T,T,T,T, x,x,x,x, x,x},
		{x,x,x,x, x,x,x,x, x,x,x,x, x,T,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, x,x,x,x, T,T,T,T, x,x,T,x, x,x}

	};
} // end Parser


public class Errors {
	public int count = 0;                                    // number of errors detected
	public System.IO.TextWriter errorStream = Console.Out;   // error messages go to this stream
	public string errMsgFormat = "-- line {0} col {1}: {2}"; // 0=line, 1=column, 2=text

	public virtual void SynErr (int line, int col, int n) {
		string s;
		switch (n) {
			case 0: s = "EOF expected"; break;
			case 1: s = "ident expected"; break;
			case 2: s = "number expected"; break;
			case 3: s = "str expected"; break;
			case 4: s = "\"+\" expected"; break;
			case 5: s = "\"-\" expected"; break;
			case 6: s = "\".\" expected"; break;
			case 7: s = "\"[\" expected"; break;
			case 8: s = "\"]\" expected"; break;
			case 9: s = "\"true\" expected"; break;
			case 10: s = "\"false\" expected"; break;
			case 11: s = "\"*\" expected"; break;
			case 12: s = "\"/\" expected"; break;
			case 13: s = "\"void\" expected"; break;
			case 14: s = "\"(\" expected"; break;
			case 15: s = "\")\" expected"; break;
			case 16: s = "\"{\" expected"; break;
			case 17: s = "\"}\" expected"; break;
			case 18: s = "\"=\" expected"; break;
			case 19: s = "\"!=\" expected"; break;
			case 20: s = "\"<\" expected"; break;
			case 21: s = "\">\" expected"; break;
			case 22: s = "\"<=\" expected"; break;
			case 23: s = "\">=\" expected"; break;
			case 24: s = "\":=\" expected"; break;
			case 25: s = "\";\" expected"; break;
			case 26: s = "\"?\" expected"; break;
			case 27: s = "\":\" expected"; break;
			case 28: s = "\"Struct\" expected"; break;
			case 29: s = "\"switch\" expected"; break;
			case 30: s = "\"case\" expected"; break;
			case 31: s = "\"default\" expected"; break;
			case 32: s = "\"break\" expected"; break;
			case 33: s = "\"if\" expected"; break;
			case 34: s = "\"else\" expected"; break;
			case 35: s = "\"while\" expected"; break;
			case 36: s = "\"for\" expected"; break;
			case 37: s = "\"read\" expected"; break;
			case 38: s = "\"write\" expected"; break;
			case 39: s = "\"program\" expected"; break;
			case 40: s = "\"int\" expected"; break;
			case 41: s = "\"bool\" expected"; break;
			case 42: s = "\"string\" expected"; break;
			case 43: s = "\"char\" expected"; break;
			case 44: s = "\"const\" expected"; break;
			case 45: s = "\",\" expected"; break;
			case 46: s = "\"external\" expected"; break;
			case 47: s = "\"procedure\" expected"; break;
			case 48: s = "??? expected"; break;
			case 49: s = "invalid AddOp"; break;
			case 50: s = "invalid RelOp"; break;
			case 51: s = "invalid Factor"; break;
			case 52: s = "invalid MulOp"; break;
			case 53: s = "invalid Stat"; break;
			case 54: s = "invalid Stat"; break;
			case 55: s = "invalid Stat"; break;
			case 56: s = "invalid ConstDecl"; break;
			case 57: s = "invalid ExternDecl"; break;
			case 58: s = "invalid Type"; break;

			default: s = "error " + n; break;
		}
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}

	public virtual void SemErr (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
		count++;
	}

	public virtual void SemErr (string s) {
		errorStream.WriteLine(s);
		count++;
	}

	public virtual void Warning (int line, int col, string s) {
		errorStream.WriteLine(errMsgFormat, line, col, s);
	}

	public virtual void Warning(string s) {
		errorStream.WriteLine(s);
	}
} // Errors


public class FatalError: Exception {
	public FatalError(string m): base(m) {}
}
}