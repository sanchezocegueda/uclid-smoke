
/**
 * @author pramod
 */
package uclid {
  package smt {
    import com.microsoft.{z3 => z3}
    import java.util.HashMap;
    import scala.collection.mutable.Map
    import scala.collection.JavaConverters._
    
    /**
     * Decide validity of SMTExpr's using a Z3 sovler.
     */
    class Z3Interface(z3Ctx : z3.Context, z3Solver : z3.Solver) extends SolverInterface {
      // Member variables.
      /** The Z3 context. */
      val ctx = z3Ctx
      /** The Z3 solver. */
      val solver = z3Solver
      /* Prefix for all  tuple names. */
      val tupleNamePrefix = "__ucl_tuple_"
      /* Counter for unique tuple names. */
      var tupleNameSuffix = 0
      /** Returns a unique tuple name. */ 
      def getTupleName() : z3.Symbol = { 
        tupleNameSuffix = tupleNameSuffix + 1
        return ctx.mkSymbol(tupleNamePrefix + tupleNameSuffix)
      }
      /** Returns tuple field names. */
      val getTupleFieldNames = new Memo[Int, Array[z3.Symbol]]((n : Int) => {
        (1 to n).map((i => ctx.mkSymbol("__ucl_field" + i.toString))).toArray
      })
      
      /** Utility function to cast to subtypes of z3.AST */
      def typecastAST[T <: z3.AST](args : List[z3.AST]) : List[T] = { 
        args.map((arg => arg.asInstanceOf[T]))
      }

      // Methods to convert uclid.smt.Type values into Z3 sorts.
      val getBoolSort = new Memo[Unit, z3.BoolSort](Unit => ctx.mkBoolSort())
      val getIntSort = new Memo[Unit, z3.IntSort](Unit => ctx.mkIntSort())
      val getBitVectorSort = new Memo[Int, z3.BitVecSort]((w : Int) => ctx.mkBitVecSort(w))
      val getTupleSort = new Memo[List[Type], z3.TupleSort]((types : List[Type]) => {
        ctx.mkTupleSort(
            getTupleName(), getTupleFieldNames(types.size),
            types.map((t) => getZ3Sort(t)).toArray
        )
      })
      val getArraySort = new Memo[(List[Type], Type), z3.ArraySort]((arrayType : (List[Type], Type)) => {
        ctx.mkArraySort(getTupleSort(arrayType._1), getZ3Sort(arrayType._2))
      })
      /** Convert uclid.smt types to Z3 sorts. */
      def getZ3Sort (typ : smt.Type) : z3.Sort = {
        typ  match {
          case BoolType()       => getBoolSort()
          case IntType()        => getIntSort()
          case BitVectorType(w) => getBitVectorSort(w)
          case TupleType(ts)    => getTupleSort(ts)
          case ArrayType(rs, d) => getArraySort(rs, d)
        }
      }

      /** Create a Z3 tuple AST. */
      def getTuple(values : List[z3.AST], tupleMemberTypes : List[Type]) : z3.Expr = {
        val tupleType = TupleType.t(tupleMemberTypes)
        val tupleSort = getZ3Sort(tupleType).asInstanceOf[z3.TupleSort]
        val tupleCons = tupleSort.mkDecl()
        tupleCons.apply(typecastAST[z3.Expr](values).toSeq : _*) 
      }
      
      /** Create a boolean literal. */
      val getBoolLit = new Memo[Boolean, z3.BoolExpr](b => ctx.mkBool(b))
      
      /** Create an integer literal. */
      val getIntLit = new Memo[BigInt, z3.IntExpr](i => ctx.mkInt(i.toString))
      
      /** Create a bitvector literal. */
      val getBitVectorLit = new Memo[(BigInt, Int), z3.BitVecExpr]((arg) => ctx.mkBV(arg._1.toString, arg._2))
      
      /** Convert a smt.Symbol object into a Z3 AST. */
      def symbolToZ3 (sym : Symbol) : z3.AST = {
        abstract class ExprSort
        case class VarSort(sort : z3.Sort) extends ExprSort
        case class MapSort(ins : List[Type], out : Type) extends ExprSort
        
        val exprSort = (sym.typ) match {
          case BoolType() => VarSort(getBoolSort())
          case IntType() => VarSort(getIntSort())
          case BitVectorType(w) => VarSort(getBitVectorSort(w))
          case TupleType(ts) => VarSort(getTupleSort(ts))
          case MapType(ins, out) => MapSort(ins, out)
          case ArrayType(ins, out) => VarSort(getArraySort(ins, out))
        } 
        
        exprSort match {
          case VarSort(s) => 
            ctx.mkFreshConst(sym.id, s)
          case MapSort(ins, out) => 
            ctx.mkFuncDecl(sym.id, ins.map(getZ3Sort _).toArray, getZ3Sort(out))
        }
      }
      
      /** Convert an OperatorApplication into a Z3 AST.  */
      def opToZ3(op : Operator, args : List[z3.AST]) : Option[z3.Expr]  = {
        // These values need to be lazy so that they are only evaluated when the appropriate ctx.mk* functions
        // are called. If they were eager, the casts would fail at runtime.
        lazy val arithArgs = typecastAST[z3.ArithExpr](args)
        lazy val boolArgs = typecastAST[z3.BoolExpr](args)
        op match {
          case IntLTOp       => Some(ctx.mkLt (arithArgs(0), arithArgs(1)))
          case IntLEOp       => Some(ctx.mkLe (arithArgs(0), arithArgs(1)))
          case IntGTOp       => Some(ctx.mkGt (arithArgs(0), arithArgs(1)))
          case IntGEOp       => Some(ctx.mkGe (arithArgs(0), arithArgs(1)))
          case IntAddOp      => Some(ctx.mkAdd (arithArgs : _*))
          case IntSubOp      => Some(ctx.mkSub (arithArgs: _*))
          case IntMulOp      => Some(ctx.mkMul (arithArgs : _*))
          case NegationOp    => Some(ctx.mkNot (boolArgs(0)))
          case IffOp         => Some(ctx.mkIff (boolArgs(0), boolArgs(1)))
          case ImplicationOp => Some(ctx.mkImplies (boolArgs(0), boolArgs(1)))
          case EqualityOp    => Some(ctx.mkEq (boolArgs(0), boolArgs(1)))
          case ConjunctionOp => Some(ctx.mkAnd (boolArgs : _*))
          case DisjunctionOp => Some(ctx.mkOr (boolArgs : _*))
          case _             => None
        }
      }
      
      /** Convert an smt.Expr object into a Z3 AST.  */
      val exprToZ3 : Memo[Expr, z3.AST] = new Memo[Expr, z3.AST]((e) => {
        val z3AST : z3.AST = e match {
          case Symbol(id, typ) => 
            symbolToZ3(Symbol(id, typ))
          case OperatorApplication(op,operands) =>
            opToZ3(op, operands.map((arg) => exprToZ3(arg))).get
          case ArraySelectOperation(e, index) => {
            val arrayType = e.typ.asInstanceOf[ArrayType]
            val arrayIndexType = arrayType.inTypes
            val indexTuple = getTuple(index.map((arg) => exprToZ3(arg)), arrayIndexType)
            ctx.mkSelect(exprToZ3(e).asInstanceOf[z3.ArrayExpr], indexTuple)
          }
          case ArrayStoreOperation(e, index, value) => {
            val arrayType = e.typ.asInstanceOf[ArrayType]
            val arrayIndexType = arrayType.inTypes
            val indexTuple = getTuple(index.map((arg) => exprToZ3(arg)), arrayIndexType)
            val data = exprToZ3(value).asInstanceOf[z3.Expr]
            ctx.mkStore(exprToZ3(e).asInstanceOf[z3.ArrayExpr], indexTuple, data)
          }
          case FunctionApplication(e, args) =>
            throw new Utils.UnimplementedException("Not implemented.")
          case ITE(e,t,f) =>
            throw new Utils.UnimplementedException("Not implemented.")
          case Lambda(_,_) =>
            throw new Utils.RuntimeError("Lambdas in assertions should have been beta-reduced.")
          case IntLit(i) => getIntLit(i)
          case BitVectorLit(bv,w) => getBitVectorLit(bv, w)
          case BooleanLit(b) => getBoolLit(b)
          case _ =>
            throw new RuntimeException("Error!")
        }
        if (z3AST.isInstanceOf[z3.Expr]) z3AST.asInstanceOf[z3.Expr].simplify()
        else z3AST
      })
      
      
      /** Check whether a particular expression is satisfiable.  */      
      def check (e : Expr) : Option[Boolean] = {
        
        println("expr: " + e.toString())
        val z3Expr = exprToZ3(e)
        println("z3: " + z3Expr.toString())
        
        solver.push()
        solver.add(z3Expr.asInstanceOf[z3.BoolExpr])
        val result = solver.check()
        solver.pop()
        
        if (result == z3.Status.SATISFIABLE) {
          Some(true)
        } else if (result == z3.Status.UNSATISFIABLE) {
          Some(false)
        } else {
          None
        }
      }
      
    }
    
    object Z3Interface {
      def newInterface() : Z3Interface = {
        var cfg = new HashMap[String, String]()
        cfg.put("model", "true")
        var ctx = new z3.Context(cfg)
        var solver = ctx.mkSolver()
        return new Z3Interface(ctx, solver)
      }
    }
    
    object SMTTester
    {
      def test() : Unit = {
        var cfg = new HashMap[String, String]()
        cfg.put("model", "true")
        var ctx = new z3.Context(cfg)
        val bv4Type = ctx.mkBitVecSort(4);
        val a = ctx.mkFreshConst("a", bv4Type).asInstanceOf[z3.BitVecExpr]
        val b = ctx.mkFreshConst("b", bv4Type).asInstanceOf[z3.BitVecExpr]
        val zero = ctx.mkNumeral(0, bv4Type).asInstanceOf[z3.BitVecExpr]
        val ones = ctx.mkBVNot(zero)
    
        val s = ctx.mkSolver()
        s.add(ctx.mkDistinct(a, zero))
        s.add(ctx.mkDistinct(b, zero))
        s.add(ctx.mkDistinct(a, ones))
        s.add(ctx.mkDistinct(b, ones))
        s.add(ctx.mkDistinct(a, b))
        s.add(ctx.mkEq(ctx.mkBVOR(a, b), ones))
        val result = s.check()
        println ("result = " + result)
        if (result == z3.Status.SATISFIABLE) {
            println("model = " + s.getModel.toString)
        }
      }
      
      def testInts() : Unit = {
        var cfg = new HashMap[String, String]()
        cfg.put("model", "true")
        var ctx = new z3.Context(cfg)
        val intSort = ctx.mkIntSort()
        val a = ctx.mkFreshConst("a", intSort).asInstanceOf[z3.ArithExpr]
        val b = ctx.mkFreshConst("b", intSort).asInstanceOf[z3.ArithExpr]
        val zero = ctx.mkNumeral(0, intSort).asInstanceOf[z3.ArithExpr]
        val ten = ctx.mkNumeral(10, intSort).asInstanceOf[z3.ArithExpr]
        
        val args = List(a, b, b, a)
        val func = ctx.mkAdd _
    
        val s = ctx.mkSolver()
        s.add(ctx.mkDistinct(a, zero))
        s.add(ctx.mkDistinct(b, zero))
        s.add(ctx.mkGt(a, zero))
        s.add(ctx.mkGt(b, zero))
        s.add(ctx.mkEq(func(args : _*), ten))
        val result = s.check()
        println ("result = " + result)
        if (result == z3.Status.SATISFIABLE) {
            println("model = " + s.getModel.toString)
        }
        
      }
    }
  }
}