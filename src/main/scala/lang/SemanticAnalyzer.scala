/*
 * UCLID5 Verification and Synthesis Engine
 *
 * Copyright (c) 2017.
 * Sanjit A. Seshia, Rohit Sinha and Pramod Subramanyan.
 *
 * All Rights Reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 1. Redistributions of source code must retain the above copyright notice,
 *
 * this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 *
 * documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holder nor the names of its
 * contributors may be used to endorse or promote products derived from this
 * software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * Authors: Rohit Sinha, Pramod Subramanyan
 *
 * Walks through the module looking for semantic errors.
 */
package uclid;
package lang;

case class ModuleError(msg : String, position : ASTPosition)

object SemanticAnalyzerPass {
  def checkIdRedeclaration(idSeq : Seq[(Identifier, ASTPosition)], in : List[ModuleError]) : List[ModuleError] = {
    val init = (Map.empty[Identifier, ASTPosition], in)
    (idSeq.foldLeft(init){
      (acc, id) => {
        acc._1.get(id._1) match {
          case Some(pos) =>
            val msg = "Redeclaration of identifier '" + id._1.name + "'. Previous declaration at " + pos.toString
            (acc._1, ModuleError(msg, id._2) :: acc._2)
          case None =>
            ((acc._1 + (id._1 -> id._2)), acc._2)
        }
      }
    })._2
  }
}

class SemanticAnalyzerPass extends ReadOnlyPass[List[ModuleError]] {
  override def applyOnModule(d : TraversalDirection.T, module : Module, in : List[ModuleError], context : Scope) : List[ModuleError] = {
    if (d == TraversalDirection.Down) {
      // val moduleIds = module.decls.filter((d) => d.declNames.isDefined).map((d) => (d.declName.get, d.position))
      val moduleIds = module.decls.flatMap((d) => d.declNames.map((n) => (n, d.position)))
      SemanticAnalyzerPass.checkIdRedeclaration(moduleIds, in)
    } else { in }
  }
  override def applyOnProcedure(d : TraversalDirection.T, proc : ProcedureDecl, in : List[ModuleError], context : Scope) : List[ModuleError] = {
    if (d == TraversalDirection.Down) {
      val inParams = proc.sig.inParams.map((arg) => (arg._1, arg._1.position))
      val outParams = proc.sig.outParams.map((arg) => (arg._1, arg._1.position))
      val localVars = proc.decls.map((v) => (v.id, v.position))
      SemanticAnalyzerPass.checkIdRedeclaration(inParams ++ outParams ++ localVars, in)
    } else { in }
  }
  override def applyOnFunction(d : TraversalDirection.T, func : FunctionDecl, in : List[ModuleError], context : Scope) : List[ModuleError] = {
    if (d == TraversalDirection.Down) {
      val params = func.sig.args.map((arg) => (arg._1, arg._1.position))
      SemanticAnalyzerPass.checkIdRedeclaration(params, in)
    } else { in }
  }
  override def applyOnRecordType(d : TraversalDirection.T, recordT : RecordType, in : List[ModuleError], context : Scope) : List[ModuleError] = {
    if (d == TraversalDirection.Down) {
      val fieldNames = recordT.members.map((f) => (f._1, f._1.position))
      SemanticAnalyzerPass.checkIdRedeclaration(fieldNames, in)
    } else {
      in
    }
  }
}

class SemanticAnalyzer extends ASTAnalyzer("SemanticAnalyzer", new SemanticAnalyzerPass())  {
  override def visit(module : Module, context : Scope) : Option[Module] = {
    val out = visitModule(module, List.empty[ModuleError], context)
    if (out.size > 0) {
      val errors = out.map((me) => (me.msg, me.position))
      throw new Utils.ParserErrorList(errors)
    }
    return Some(module)
  }
}
