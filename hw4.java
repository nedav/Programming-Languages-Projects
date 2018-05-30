/* Other Resources I Consulted:
   https://docs.oracle.com/javase/7/docs/api/java/util/Stack.html
   https://docs.oracle.com/javase/7/docs/api/java/util/Comparator.html
   http://www.tutorialspoint.com/java/java_string_compareto.htm
   http://www.tutorialspoint.com/java/java_generics.htm
*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
    double eval(); 	                       // Problem 1a
    List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    // Constructor
    public Num(double val) { this.val = val; }

    // Implementation of eval
    public double eval() { return val; }

    // Returns a list of just a Push
    public List<Instr> compile() {
    	List<Instr> l = new LinkedList<Instr>();
		l.add(new Push(val));
		return l;
    }

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    // Constructor
    public BinOp(Exp left, Op op, Exp right) {
    	this.left = left;
    	this.op = op;
    	this.right = right;
    }

    // Calculate the result of evaluating the left and right sides
    public double eval() {
    	return op.calculate (left.eval(), right.eval());
    }

    // Returns a list of the left side compiled, the right side compiled, and a Calculate for the operation
    public List<Instr> compile() {
    	List<Instr> l = new LinkedList<Instr>();
		l.addAll(left.compile());
		l.addAll(right.compile());
		l.add(new Calculate(op));
		return l;
    }

    public boolean equals(Object o) {
    	if(!(o instanceof BinOp))
    		return false;
    	BinOp b = (BinOp) o;
    	return this.left.equals(b.left) && this.op.equals(b.op) &&
		    	this.right.equals(b.right);
    }

    public String toString() {
		return "BinOp(" + left + ", " + op + ", " + right + ")";
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for arithmetic instructions
interface Instr {
	// Function that Instrs.execute can call to evaluate an Instr
	Stack<Double> eval(Stack<Double> stack);
}

class Push implements Instr {
    protected double val;

    // Constructor
    public Push(double val) { this.val = val; }

    // Pushes the value onto the stack
    public Stack<Double> eval(Stack<Double> stack)
    {
    	stack.push(val);
    	return stack;
    }

	public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
		return "Push " + val;
    }

}

class Calculate implements Instr {
    protected Op op;

    // Constructor
    Calculate(Op op) { this.op = op; }

    // Pops the top two elements of the stack, calculates the result of applying the operation, and pushes the result onto the stack
    public Stack<Double> eval(Stack<Double> stack)
    {
    	double a = stack.pop();
    	double b = stack.pop();
    	stack.push(op.calculate(b, a));
    	return stack;
    }

    public boolean equals(Object o) { return (o instanceof Calculate) && 
    						  ((Calculate)o).op.equals(this.op); }

    public String toString() {
		return "Calculate " + op;
    }    
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

	// For every instruction, call its evaluate function
    public double execute() {
    	Stack<Double> stack = new Stack<Double>();
    	for (Instr i : instrs)
    	{
    		stack = i.eval(stack);
    	}
    	// The top of the stack contains a Num that evaluates to the result
    	return stack.peek();
    }
}

class CalcTest {
    public static void main(String[] args) {
    	// Problem 1a Tests //
    	Exp n = new Num(3.0);
    	assert(n.eval() == 3.0);
    	// (1 + 2) * 3
		Exp exp =
	     	new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
		     	  	  Op.TIMES,
		       	  new Num(3.0));
		assert(exp.eval() == 9.0);
		// (5 - 3) * 3
		Exp exp1 =
	     	new BinOp(new BinOp(new Num(5.0), Op.MINUS, new Num(3.0)),
		     	  	  Op.TIMES,
		       	  new Num(3.0));
		assert(exp1.eval() == 6.0);
		// (5 * 3) / 3
		Exp exp2 =
	     	new BinOp(new BinOp(new Num(5.0), Op.TIMES, new Num(3.0)),
		     	  	  Op.DIVIDE,
		       	  new Num(3.0));
		assert(exp2.eval() == 5.0);
		// (6 / 3) + 40
		Exp exp3 =
	     	new BinOp(new BinOp(new Num(6.0), Op.DIVIDE, new Num(3.0)),
		     	  	  Op.PLUS,
		       	  new Num(40.0));
		assert(exp3.eval() == 42.0);
		// (5 - 3) - ((12 / 3) * 3)
		Exp exp4 =
	     	new BinOp(new BinOp(new Num(5.0), Op.MINUS, new Num(3.0)),
		     	  	  Op.MINUS,
		       	  new BinOp(new BinOp(new Num(12.0), Op.DIVIDE, new Num(3.0)), 
		       	  		Op.TIMES, new Num(3.0)));
		assert(exp4.eval() == -10.0);

		// Problem 1b Tests //
		// (1 + 2) * 3
		List<Instr> is = new LinkedList<Instr>();
		is.add(new Push(1.0));
		is.add(new Push(2.0));
		is.add(new Calculate(Op.PLUS));
		is.add(new Push(3.0));
		is.add(new Calculate(Op.TIMES));
		Instrs instrs = new Instrs(is);
		assert(instrs.execute() == 9.0);
		// (5 - 3) * 3
		List<Instr> is1 = new LinkedList<Instr>();
		is1.add(new Push(5.0));
		is1.add(new Push(3.0));
		is1.add(new Calculate(Op.MINUS));
		is1.add(new Push(3.0));
		is1.add(new Calculate(Op.TIMES));
		Instrs instrs1 = new Instrs(is1);	
		assert(instrs1.execute() == 6.0);
		// (5 * 3) / 3
		List<Instr> is2 = new LinkedList<Instr>();
		is2.add(new Push(5.0));
		is2.add(new Push(3.0));
		is2.add(new Calculate(Op.TIMES));
		is2.add(new Push(3.0));
		is2.add(new Calculate(Op.DIVIDE));
		Instrs instrs2 = new Instrs(is2);	
		assert(instrs2.execute() == 5.0);
		// (6 / 3) + 40
		List<Instr> is3 = new LinkedList<Instr>();
		is3.add(new Push(6.0));
		is3.add(new Push(3.0));
		is3.add(new Calculate(Op.DIVIDE));
		is3.add(new Push(40.0));
		is3.add(new Calculate(Op.PLUS));
		Instrs instrs3 = new Instrs(is3);	
		assert(instrs3.execute() == 42.0);
		// (5 - 3) - ((12 / 3) * 3)
		List<Instr> is4 = new LinkedList<Instr>();
		is4.add(new Push(5.0));
		is4.add(new Push(3.0));
		is4.add(new Calculate(Op.MINUS));
		is4.add(new Push(12.0));
		is4.add(new Push(3.0));
		is4.add(new Calculate(Op.DIVIDE));
		is4.add(new Push(3.0));
		is4.add(new Calculate(Op.TIMES));
		is4.add(new Calculate(Op.MINUS));
		Instrs instrs4 = new Instrs(is4);	
		assert(instrs4.execute() == -10.0);
		// More tests
		// 1 - 2
		List<Instr> isa = new LinkedList<Instr>();
		isa.add(new Push(1.0));
		isa.add(new Push(2.0));
		isa.add(new Calculate(Op.MINUS));
		Instrs instrsa = new Instrs(isa);
		assert(instrsa.execute() == -1.0);
		// 15 / 3
		List<Instr> isb = new LinkedList<Instr>();
		isb.add(new Push(15.0));
		isb.add(new Push(3.0));
		isb.add(new Calculate(Op.DIVIDE));
		Instrs instrsb = new Instrs(isb);
		assert(instrsb.execute() == 5.0);
		// (15 / 3) * 5
		List<Instr> isc = new LinkedList<Instr>();
		isc.add(new Push(15.0));
		isc.add(new Push(3.0));
		isc.add(new Calculate(Op.DIVIDE));
		isc.add(new Push(5.0));
		isc.add(new Calculate(Op.TIMES));
		Instrs instrsc = new Instrs(isc);		
		assert(instrsc.execute() == 25.0);

		// Problem 1c Tests //
		assert(exp.compile().equals(is));
		assert(exp1.compile().equals(is1));
		assert(exp2.compile().equals(is2));
		assert(exp3.compile().equals(is3));
		assert(exp4.compile().equals(is4));

		// Problem 2a Tests //
		ListStringSet a = new ListStringSet();
		assert(a.size() == 0);
		assert(a.contains("Bob") == false);
		a.add("Bob");
		assert(a.size() == 1);
		a.add("Anne");
		assert(a.toString().equals("SElement(Anne, SElement(Bob, SEmpty))"));
		assert(a.contains("Bob") == true);
		assert(a.contains("Anne") == true);
		a.add("Bob");
		assert(a.toString().equals("SElement(Anne, SElement(Bob, SEmpty))"));
		a.add("Anne");
		assert(a.toString().equals("SElement(Anne, SElement(Bob, SEmpty))"));
		a.add("Bill");
		assert(a.toString().equals("SElement(Anne, SElement(Bill, SElement(Bob, SEmpty)))"));
		assert(a.contains("Frank") == false);
		a.add("Frank");
		assert(a.toString().equals("SElement(Anne, SElement(Bill, SElement(Bob, SElement(Frank, SEmpty))))"));
		assert(a.size() == 4);

		StringSet s = new ListStringSet();
        assert(s.size() == 0);
        assert(!s.contains(""));
        s.add("x");
        assert(s.contains("x"));
        assert(s.size() == 1);
        s.add("x");
        assert(s.contains("x"));
        assert(s.size() == 1);

        Set<String> t = new ListSet<String>((s1, s2) -> s2.compareTo(s1));
        assert(t.size() == 0);
        assert(!t.contains(""));
        t.add("x");
        assert(t.contains("x"));
        assert(t.size() == 1);
        t.add("x");
        assert(t.contains("x"));
        assert(t.size() == 1);

		// Problem 2b Tests //
		ListSet<String> b = new ListSet<String>((String s1, String s2) -> s1.compareTo(s2));
		assert(b.size() == 0);
		assert(b.contains("Bob") == false);
		b.add("Bob");
		assert(b.size() == 1);
		b.add("Anne");
		assert(b.toString().equals("Element(Anne, Element(Bob, Empty))"));
		assert(b.contains("Bob") == true);
		assert(b.contains("Anne") == true);
		b.add("Bob");
		assert(b.toString().equals("Element(Anne, Element(Bob, Empty))"));
		b.add("Anne");
		assert(b.toString().equals("Element(Anne, Element(Bob, Empty))"));
		b.add("Bill");
		assert(b.toString().equals("Element(Anne, Element(Bill, Element(Bob, Empty)))"));
		assert(b.contains("Frank") == false);
		b.add("Frank");
		assert(b.toString().equals("Element(Anne, Element(Bill, Element(Bob, Element(Frank, Empty))))"));
		assert(b.size() == 4);

		ListSet<String> c = new ListSet<String>((String s1, String s2) -> s2.compareTo(s1));
		c.add("Bob");
		c.add("Anne");
		c.add("Bob");
		c.add("Anne");
		c.add("Bill");
		c.add("Frank");
		assert(c.toString().equals("Element(Frank, Element(Bob, Element(Bill, Element(Anne, Empty))))"));
		assert(c.size() == 4);

		ListSet<Integer> d = new ListSet<Integer> ((Integer i1, Integer i2) -> i1 - i2);
		d.add(0);
		d.add(-5);
		assert(d.contains(5) == false);
		d.add(5);
		d.add(5);
		d.add(0);
		d.add(-5);
		assert(d.toString().equals("Element(-5, Element(0, Element(5, Empty)))"));
		assert(d.size() == 3);
		assert(d.contains(0) == true);
    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
    int size();
    boolean contains(String s);
    void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    protected SNode head;

    // Constructor intializes to an empty list
    ListStringSet() {
    	head = new SEmpty();
    }

    public int size() {
    	return head.size();
    }

    public boolean contains(String s) {
    	return head.contains(s);
    }

    // add returns a new linked list that is the result of adding the element to the set
    public void add(String s) {
    	head = head.add(s);
    }

    public String toString() {
    	return head.toString();
    }
}

// a type for the nodes of the linked list
interface SNode {
	int size();
	boolean contains(String s);
	SNode add(String s);
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
	// No elements, so return 0
	public int size() { return 0; }

	// If the loop has gotten to the end of the list, s cannot be in the list
	public boolean contains(String s) { return false; }

	// Simply add the element in front of the empty node
	public SNode add(String s) {
		return new SElement(s, this);
	}

    public String toString() {
		return "SEmpty";
	}
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;

    SElement(String elem, SNode next) {
    	this.elem = elem;
    	this.next = next; 
    }

    // For every Element, getSize will add 1, for the Empty node, getSize will add 0
    public int size() {
		return 1 + next.size();
    }

	public boolean contains(String s) {
		int num = elem.compareTo(s);
		// If the strings are the same, the set contains the string
		if (num == 0)
			return true;
		// If s is smaller, it can't be in the list because it would have been found by now
		else if (num > 0)
			return false;
		// If s is greater, it could still be in the list
		else
			return next.contains(s);
	}    

	public SNode add(String s) {
		int num = elem.compareTo(s);
		// If the strings are the same, don't add it
		if (num == 0)
			return this;
		// If s is smaller, add s before the current node
		else if (num > 0)
			return new SElement(s, this);
		// If s is greater, continue traversing the list
		else
			return new SElement(elem, next.add(s));
	}

    public String toString() {
		return "SElement(" + elem + ", " + next.toString() + ")";
	}
}


// Problem 2b
interface Set<T> {
	int size();
    boolean contains(T t);
    void add(T t);
}

class ListSet<T> implements Set<T> {
	protected Node<T> head;
	protected Comparator<T> comp;
	
    // Constructor
    ListSet(Comparator<T> comp) {
    	head = new Empty<T>(comp);
    	this.comp = comp;
    }

    public int size() {
    	return head.size();
    }

    public boolean contains(T t) {
    	return head.contains(t);
    }

	// add returns a new linked list that is the result of adding the element to the set
    public void add(T t) {
    	head = head.add(t);
    }

    public String toString() {
    	return head.toString();
    }

}


interface Node<T> {
	int size();
	boolean contains(T t);
	Node<T> add(T t);
}

class Empty<T> implements Node<T> {
	protected Comparator<T> comp;
	// Constructor 
	Empty(Comparator<T> comp) {
		this.comp = comp;
	}
	// No elements, so return 0
	public int size() { return 0; }

	// If the loop has gotten to the end of the list, s cannot be in the list (or the list is empty)
	public boolean contains(T t) { return false; }

	// Simply add the element in front of the empty node
	public Node<T> add(T t) {
		return new Element<T>(t, this, comp);
	}

    public String toString() {
		return "Empty";
	}
}

class Element<T> implements Node<T> {
	protected T elem;
    protected Node<T> next;
    protected Comparator<T> comp;

	Element(T elem, Node<T> next, Comparator<T> comp) {
    	this.elem = elem;
    	this.next = next; 
    	this.comp = comp;
    }

    // For every Element, getSize will add 1, for the Empty node, getSize will add 0
    public int size() {
		return 1 + next.size();
    }

	public boolean contains(T t) {
		int num = comp.compare(elem, t);
		// If the strings are the same, the set contains the string
		if (num == 0)
			return true;
		// If s is smaller, it can't be in the list because it would have been found by now
		else if (num > 0)
			return false;
		// If s is greater, it could still be in the list
		else
			return next.contains(t);
	}    

	public Node<T> add(T t) {
		int num = comp.compare(elem, t);
		// If the strings are the same, don't add it
		if (num == 0)
			return this;
		// If s is smaller, add s before the current node
		else if (num > 0)
			return new Element<T>(t, this, comp);
		// If s is greater, continue traversing the list
		else
			return new Element<T>(elem, next.add(t), comp);
	}

    public String toString() {
		return "Element(" + elem + ", " + next.toString() + ")";
	}
}