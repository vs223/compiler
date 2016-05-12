//------------------------------------------------------------------------------
/// @brief SnuPL/0 parser
/// @author Bernhard Egger <bernhard@csap.snu.ac.kr>
/// @section changelog Change Log
/// 2012/09/14 Bernhard Egger created
/// 2013/03/07 Bernhard Egger adapted to SnuPL/0
/// 2014/11/04 Bernhard Egger maintain unary '+' signs in the AST
/// 2016/04/01 Bernhard Egger adapted to SnuPL/1 (this is not a joke)
/// 2016/09/28 Bernhard Egger assignment 2: parser for SnuPL/-1
///
/// @section license_section License
/// Copyright (c) 2012-2016, Bernhard Egger
/// All rights reserved.
///
/// Redistribution and use in source and binary forms,  with or without modifi-
/// cation, are permitted provided that the following conditions are met:
///
/// - Redistributions of source code must retain the above copyright notice,
///   this list of conditions and the following disclaimer.
/// - Redistributions in binary form must reproduce the above copyright notice,
///   this list of conditions and the following disclaimer in the documentation
///   and/or other materials provided with the distribution.
///
/// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
/// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING,  BUT NOT LIMITED TO,  THE
/// IMPLIED WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE
/// ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDER  OR CONTRIBUTORS BE
/// LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSE-
/// QUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF  SUBSTITUTE
/// GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
/// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN  CONTRACT, STRICT
/// LIABILITY, OR TORT  (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING IN ANY WAY
/// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
/// DAMAGE.
//------------------------------------------------------------------------------

#include <limits.h>
#include <cassert>
#include <errno.h>
#include <cstdlib>
#include <vector>
#include <iostream>
#include <exception>
#include <stdio.h>

#include "parser.h"

#define PeekType() _scanner->Peek().GetType()
#define trace()
using namespace std;

//todo
//symbol table check
//ETokendd base type Null to tundefined
//


//------------------------------------------------------------------------------
// CParser
//

CParser::CParser(CScanner *scanner)
{
  _scanner = scanner;
  _module = NULL;
}

CAstNode* CParser::Parse(void)
{
  _abort = false;

  if (_module != NULL) { delete _module; _module = NULL; }

  try {
    if (_scanner != NULL) _module = module();

    if (_module != NULL) {
      CToken t;
      string msg;
      if (!_module->TypeCheck(&t, &msg)) 
        SetError(t, msg);
    }
  } catch (...) {
    _module = NULL;
  }

  return _module;
}

const CToken* CParser::GetErrorToken(void) const
{
  if (_abort) return &_error_token;
  else return NULL;
}

string CParser::GetErrorMessage(void) const
{
  if (_abort) return _message;
  else return "";
}

void CParser::SetError(CToken t, const string message)
{
  _error_token = t;
  _message = message;
  _abort = true;
  throw message;
}

bool CParser::Consume(EToken type, CToken *token)
{
  if (_abort) return false;

  CToken t = _scanner->Get();

  if (t.GetType() != type) {
    SetError(t, "expected '" + CToken::Name(type) + "', got '" +
             t.GetName() + "'");
  }

  if (token != NULL) *token = t;

  return t.GetType() == type;
}

void CParser::InitSymbolTable(CSymtab *s)
{
  CTypeManager *tm = CTypeManager::Get();

  CSymProc* symproc = new CSymProc("DIM", tm->GetInt());
  symproc->AddParam(new CSymParam(0, "arg0", tm->GetPointer(tm->GetNull())));
  symproc->AddParam(new CSymParam(1, "arg1", tm->GetInt()));
  s->AddSymbol(symproc);

  symproc = new CSymProc("DOFS", tm->GetInt());
  symproc->AddParam(new CSymParam(0, "arg", tm->GetPointer(tm->GetNull())));
  s->AddSymbol(symproc);

  symproc = new CSymProc("ReadInt", tm->GetInt());
  s->AddSymbol(symproc);

  symproc = new CSymProc("WriteChar", tm->GetNull());
  symproc->AddParam(new CSymParam(0, "arg", tm->GetChar()));
  s->AddSymbol(symproc);

  symproc = new CSymProc("WriteInt", tm->GetNull());
  symproc->AddParam(new CSymParam(0, "arg", tm->GetInt()));
  s->AddSymbol(symproc);

  symproc = new CSymProc("WriteLn", tm->GetNull());
  s->AddSymbol(symproc);

  symproc = new CSymProc("WriteStr", tm->GetNull());
  symproc->AddParam(new CSymParam(0, "arg", tm->GetPointer(tm->GetArray(CArrayType::OPEN, tm->GetChar()))));
  s->AddSymbol(symproc);
}

CAstModule* CParser::module(void)
{
  //
  // module ::= "module" ident ";" varDeclaration { subroutineDecl }"begin" statSequence "end" ident ".". 
  //
  CToken t, name,name2;
  Consume(tModule, &t);    
  Consume(tIdent, &name);
  Consume(tSemicolon);
  CAstModule *m = new CAstModule(t, name.GetValue());
  InitSymbolTable(m->GetSymbolTable());

  varDeclaration(m);
  while(PeekType()!=tBegin){
    subroutineDecl(m);
  }
  Consume(tBegin);

  CAstStatement *statseq = NULL;
  statseq = statSequence(m);

  Consume(tEnd);
  Consume(tIdent, &name2);
  if(name.GetValue()!= name2.GetValue()){
    SetError(name2, name.GetValue()+"!="+name2.GetValue());
  }
  Consume(tDot);

  m->SetStatementSequence(statseq);

  return m;
}

CAstProcedure *CParser::subroutineDecl(CAstScope *s)
{
  //
  //subroutineDecl ::=(procedureDecl | functionDecl) subroutineBody ident ";".
  //for some technical problem, this absorbed procedureDecl and functionDecl
  //procedureDecl ::= "procedure" ident [ formalParam ] ";".
  //functionDecl ::= "function" ident [ formalParam ] ":" type ";".
  //
  CToken t, t2;
  EToken tt = PeekType();
  CSymProc *symproc = NULL;

  if(tt == tProcedure)
    Consume(tProcedure);
  else
    Consume(tFunction);

  //Consume tIdnet
  Consume(tIdent, &t);
  if(s->GetSymbolTable()->FindSymbol( t.GetValue()) != NULL)
    SetError(t, "Duplcated definition.");

  symproc =  new CSymProc(t.GetValue(), CTypeManager::Get()->GetNull());
  CAstProcedure *proScope = new CAstProcedure( t, t.GetValue(),s, symproc);

  if(tt == tProcedure){
    if(PeekType() != tSemicolon) //next token is first of follow or next token is not follow of this
      formalParam(proScope, symproc);
  }
  else{
    if(PeekType() != tColon) //next token is first of follow or next token is not follow of this
      formalParam(proScope, symproc);
    Consume(tColon);
    CAstType *_retType = type(s);
    CSymProc *symfunc =  new CSymProc(t.GetValue(), _retType->GetType());
    const CDataInitializer  * di = symproc->GetData();
    symfunc->SetData(di);
    /*  for(int i = 0; i < symproc->GetNParams();i++){
        const CDataInitializer  * di = symproc->GetParam(i)->GetData();
        CSymParam * symToAdd = new CSymParam(p->);
        symfunc->AddParam(symToAdd);
        }*/
    symproc = symfunc;
    proScope->SetSymbol(symproc);
  }
  Consume(tSemicolon);

  //trace();
  s->GetSymbolTable()->AddSymbol(symproc);

  CAstStatement *stsq = subroutineBody(proScope);
  //trace();
  proScope->SetStatementSequence(stsq);

  Consume(tIdent, &t2);

  if(t.GetValue()!= t2.GetValue()){
    //procedure name check
    SetError(t, "Procedure | function name mismatch : " + t.GetValue()+"!="+t2.GetValue());
  }
  Consume(tSemicolon);


  return proScope;
}

void CParser::formalParam(CAstScope *s, CSymProc *p){
  //
  //formalParam = "(" [ varDeclSequence ] ")".
  //
  Consume(tLParens);
  if(PeekType()!= tRParens)
    varDeclSequence(s, p);
  Consume(tRParens);
}

CAstStatement *CParser::subroutineBody(CAstScope *s ){
  //
  //subroutineBody ::= varDeclaration "begin" statSequence "end".
  //
  varDeclaration(s);
  Consume(tBegin);
  CAstStatement *stsq = statSequence(s);
  Consume(tEnd);
  return stsq;
}




void CParser::varDeclaration(CAstScope *s)
{
  //
  //varDeclaration = [ "var" varDeclSequence ";" ].
  //               = [ "var" varDecl { ";" varDecl } ";"}].
  //               = [ "var" varDecl ";" {varDecl ";"}].
  //Not use varDeclSequence to avoid ambiguity.
  //FOLLOW = {tBegin, tProcedure, tFunction}.
  //FIRST(varDecl) = {tIdent}.
  //

if(PeekType() == tVarDecl){
  bool rear_flag = 1;
  Consume(tVarDecl);
  varDecl(s);
  Consume(tSemicolon);

  while(rear_flag){

    switch(PeekType()){
      case tBegin:
      case tProcedure:
      case tFunction:
        rear_flag = 0;
        break;
      case tIdent:
        rear_flag = 1;
        break;
      default:
        //unexpected token, probably dying after this mehtod
        break;
    }

    if(rear_flag == 0)
      break;
    varDecl(s);
    Consume(tSemicolon);
  }
}
}


void CParser::varDeclSequence(CAstScope *s, CSymProc *p)
{
  //
  //varDeclSequence = varDecl { ";" varDecl }.
  //callers of this are varDeclaration and formalParam
  //Because of ambiguity, only formalParam use this one
  //
  varDecl(s, p);
  while(PeekType() == tSemicolon ){   
    Consume(tSemicolon);
    varDecl(s, p);
  }
}

void CParser::varDecl(CAstScope *s, CSymProc *p)
{
  //
  //varDecl ::= ident { "," ident } ":" type.
  //duplicated def error is checked when token is uploaded to symTab to avoid double check
  //FIRST = {tIdent}
  //
  CToken t;
  CSymtab *symtab = s->GetSymbolTable();
  std::vector<CToken> tokenBuf;

  Consume(tIdent, &t);
  tokenBuf.push_back(t);
  while(PeekType() == tComma){
    Consume(tComma);
    Consume(tIdent, &t);
    tokenBuf.push_back(t);
  }
  Consume(tColon);
  CAstType * astType = type(s);
  const CType* _type;
  if(p!=NULL && astType->GetType()->IsArray()){
    _type = CTypeManager::Get()->GetPointer(astType->GetType());
  }
  else{
    _type = astType->GetType();
  }
  int vsize = tokenBuf.size();
  for(int i = 0; i < vsize; i++){
    if(symtab-> FindSymbol(tokenBuf[i].GetValue(), sLocal)!=NULL){
      SetError(t, "Duplicated definition.");
    }
    if(p!=NULL){
      CSymParam *symToAdd = new CSymParam(p->GetNParams(), tokenBuf[i].GetValue(),_type);
      if (!(symtab->AddSymbol(symToAdd)))  
        SetError(t, "fail to add token to symboltable");           
      p->AddParam(symToAdd);
    }
    else{
      CSymbol *symbolToAdd = s->CreateVar(tokenBuf[i].GetValue(), _type);
      if(!symtab->AddSymbol(symbolToAdd))
        SetError(t, "fail to add token to symboltable");
    }

  }
  tokenBuf.clear();
}

CAstType * CParser::type(CAstScope *s)
{
  //
  //type ::= basetype | type "[" [ number ] "]".
  //basetype ::= "boolean" | "char" | "integer".
  //FIRST ::= {"boolean", "char", "integer"}
  //
  CToken t;
  EToken tt = PeekType();
  const CType *_type = NULL;
  switch(tt){
    case tBoolean:
      Consume(tBoolean, &t);
      _type = CTypeManager::Get()->GetBool();
      break;
    case tChar:
      Consume(tChar ,&t);
      _type = CTypeManager::Get()->GetChar();
      break;
    case tInteger:
      Consume(tInteger, &t);
      _type = CTypeManager::Get()->GetInt();
      break;
    default:
      t = _scanner->Get();
      SetError(t, "expected '" + CToken::Name(tBoolean)+" | "
               + CToken::Name(tChar)+" | "
               + CToken::Name(tInteger) + "', got '" +
               t.GetValue() + "'");
      break;
  }
  while(PeekType() == tLBrak){
    CAstConstant *num = NULL;
    Consume(tLBrak);
    if(PeekType() == tNumber)
      num = number();
    Consume(tRBrak);
    if(num == NULL)
      _type = new CArrayType(CArrayType::OPEN,_type);
    else
      _type = new CArrayType(num->GetValue(),_type);
  }

  return new CAstType(t, _type);
}



CAstStatement* CParser::statSequence(CAstScope *s)
{
  //
  // statSequence ::= [ statement { ";" statement } ].
  // statement ::= assignment | subroutineCall | ifStatement | whileStatement | returnStatement.
  // FIRST(statSequence) = { tIdent, tIf , tWhile, tReturn }
  // FOLLOW(statSequence) = { tElse ,tEnd }
  //
  CAstStatement *head = NULL;

  EToken tt = _scanner->Peek().GetType();
  if (!(tt == tEnd || tt==tElse)) {                    // !FOLLOW
    CAstStatement *tail = NULL;

    do {
      CToken t;
      EToken tt = _scanner->Peek().GetType();
      CAstStatement *st = NULL;

      switch (tt) {
        // statement ::= assignment | subroutineCall
        case tIdent:
          Consume(tIdent, &t);
          if(PeekType() == tLParens){
            st= subroutineCall(s, &t);
          }
          else{
            st = assignment(s, &t);
          }
          break;
          // statement ::= ifStatement
        case tIf:
          st = ifStatement(s);
          break;
          // statement ::= whileStatement
        case tWhile:
          st = whileStatement(s);
          break;
          // statement ::= returnStatement
        case tReturn:
          st = returnStatement(s);
          break;

        default:
          SetError(_scanner->Peek(), "statement expected.");
          break;
      }

      assert(st != NULL);
      if (head == NULL) head = st;
      else tail->SetNext(st);
      tail = st;

      tt = _scanner->Peek().GetType();
      if (tt == tEnd || tt == tElse) break;                  //FOLLOW

      Consume(tSemicolon);
    } while (!_abort);
  }

  return head;
}

CAstStatAssign* CParser::assignment(CAstScope *s, CToken *_t )
{
  //
  // assignment ::= qualident ":=" expression.
  //

  CToken t;
  CAstDesignator *lhs = qualident(s , _t);
  Consume(tAssign, &t);

  CAstExpression *rhs = expression(s);
  return new CAstStatAssign(t, lhs, rhs);
}

CAstStatCall* CParser::subroutineCall(CAstScope *s, CToken *_t)
{
  //
  // subroutineCall ::= ident "(" [expression { , expression } ] ")".
  // check a ident is in the symbol Table
  // if the ident consumed by caller, this gets the ident from caller by the t parameter.
  //

  CToken t;
  if(_t == NULL)
    Consume(tIdent, &t);
  else
    t = *_t;

  const CSymbol *sym_check = s->GetSymbolTable()->FindSymbol(t.GetValue());
  const CSymProc * symbol = dynamic_cast<const CSymProc *>(sym_check);

  if( symbol == NULL)
    SetError(t, "a token is not in symbol table.");

  Consume(tLParens);
  CAstFunctionCall * funCall = new CAstFunctionCall(t, symbol);
  CAstExpression * expr = NULL;

  while(_scanner->Peek().GetType()!= tRParens){
    expr = expression(s);
    
    if(expr->GetType()->IsArray()){
      funCall->AddArg(
          new CAstSpecialOp(t, opAddress, expr)
          );
    }
    else
      funCall->AddArg(expr);

    if(_scanner->Peek().GetType()==tComma){
      Consume(tComma);
    }
    else{
      break;
    }
  }
  Consume(tRParens);

  return new CAstStatCall(t, funCall);
}


CAstStatIf * CParser::ifStatement(CAstScope *s)
{
  //
  //ifStatement ::= "if" "(" expression ")" "then" statSequence [ "else" statSequence ] "end".
  //
  CToken t;

  Consume(tIf, &t);
  Consume(tLParens);

  CAstExpression *condition = expression(s);
  Consume(tRParens);
  Consume(tThen);
  CAstStatement *ifBody , *elseBody;

  ifBody = statSequence(s);

  if(PeekType() == tElse){
    Consume(tElse);
    elseBody = statSequence(s);
  }
  Consume(tEnd);            

  return new CAstStatIf(t, condition, ifBody, elseBody);
}

CAstStatWhile * CParser::whileStatement(CAstScope *s)
{
  //
  //whileStatement ::= "while" "(" expression ")" "do" statSequence "end".
  //
  CToken t;

  Consume(tWhile, &t);
  Consume(tLParens);

  CAstExpression *condition = expression(s);
  Consume(tRParens);

  Consume(tDo);
  CAstStatement *whileBody = statSequence(s);
  Consume(tEnd);

  return new CAstStatWhile(t, condition, whileBody);
}

CAstStatReturn *CParser::returnStatement(CAstScope *s)
{
  //
  // returnStatement ::= = "return" [ expression ].
  // FOLLOW ::= { ";", tEnd, tElse }
  //
  CToken t;
  Consume(tReturn, &t);
  CAstExpression *retValue = NULL;


  switch(PeekType()){             //if next token is not in follow of returnStatement, the returnValue should exist
    case tSemicolon:
    case tEnd:
    case tElse:
      break;
    default:
      retValue = expression(s);
      break;
  }

  return new CAstStatReturn(t, s, retValue);
}


CAstExpression* CParser::expression(CAstScope* s)
{
  //
  // expression ::= simpleexpr [ relOp simpleexpression ].
  // relop ::= "=" | "#" | "<" | "<=" | ">" | ">=".
  //
  CToken t;
  EOperation relop;
  CAstExpression *left = NULL, *right = NULL;

  left = simpleexpr(s);

  if (_scanner->Peek().GetType() == tRelOp) {
    Consume(tRelOp, &t);
    trace();
    right = simpleexpr(s);
    trace();
    if (t.GetValue() == "=")       relop = opEqual;
    else if (t.GetValue() == "#")  relop = opNotEqual;
    else if (t.GetValue() == "<")  relop = opLessThan;
    else if (t.GetValue() == "<=")  relop = opLessEqual;
    else if (t.GetValue() == ">")  relop = opBiggerThan;
    else if (t.GetValue() == ">=")  relop = opBiggerEqual;
    else SetError(t, "invalid relation.");

    return new CAstBinaryOp(t, relop, left, right);
  } else {
    return left;
  }
}

CAstExpression* CParser::simpleexpr(CAstScope *s)
{
  //
  // simpleexpr ::= ["+" | "-" ]term { termOp term }.
  //
  CAstExpression *n = NULL;

  CToken t;
  EToken tt = PeekType();
  if(tt == tPlusMinus)
    Consume(tPlusMinus, &t);
  n = term(s);

  if( tt == tPlusMinus){
    n = new CAstUnaryOp(t, t.GetValue() == "+" ? opPos : opNeg, n);
  }

  while (_scanner->Peek().GetType() == tPlusMinus || PeekType() == tOr)  {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(PeekType(), &t);

    r = term(s);
    EOperation op;
    if(t.GetValue() == "+")
      op =  opAdd;
    else if(t.GetValue() =="-") 
      op = opSub;
    else 
      op = opOr;

    n = new CAstBinaryOp(t, op, l, r);
  }


  return n;
}

CAstExpression* CParser::term(CAstScope *s)
{
  //
  // term ::= factor { ("*"|"/"|"&&") factor }.
  //
  CAstExpression *n = NULL;

  n = factor(s);

  EToken tt = _scanner->Peek().GetType();

  while ((tt == tMulDiv) || (tt== tAnd)) {
    CToken t;
    CAstExpression *l = n, *r;

    Consume(tt, &t);

    r = factor(s);

    EOperation op = opAnd;
    if(t.GetValue() == "*")
      op = opMul;
    else if(t.GetValue() == "/")
      op = opDiv;

    n = new CAstBinaryOp(t, op , l, r);

    tt = _scanner->Peek().GetType();
  }

  return n;
}

CAstExpression* CParser::factor(CAstScope *s)
{
  //
  // factor ::= qualident | number | boolean | char | string | "(" expression ")" | subroutineCall | "!" factor
  //
  // FIRST(factor) = { tIdent, tNumber, tBoolConst, tCharConst, tString, tLBrak, tNot }
  // 

  CToken t;
  EToken tt = _scanner->Peek().GetType();
  CAstExpression *unary = NULL, *n = NULL;

  switch (tt) {
    // factor ::= number
    case tNumber:
      n = number();
      break;
      // factor ::= boolean
    case tBoolConst:
      n = boolean();
      break;
      // factor ::= char
    case tCharConst:
      n = character();
      break;
      // factor ::= string
    case tString:
      n = _string(s);
      break;
      // factor ::= "!" factor
    case tNot:
      Consume(tNot, &t);
      n = factor(s);
      n = new CAstUnaryOp(t, opNot, n); 
      break;
      //factor ::= qualident | subroutineCall
    case tIdent:
      Consume(tIdent, &t);
      if(PeekType() == tLParens){
        n = subroutineCall(s, &t) -> GetCall();
      }
      else {
        n = qualident(s, &t);
      }
      break;            
      // factor ::= "(" expression ")"
    case tLParens:
      Consume(tLParens);
      n = expression(s);
      Consume(tRParens);
      break;


    default:
      cout << "got " << _scanner->Peek() << endl;
      SetError(_scanner->Peek(), "factor expected.");
      break;
  }

  return n;
}

CAstDesignator* CParser::qualident(CAstScope *s, CToken *_t)
{
  //
  // qualident ::= ident { "[" expression "]" }.
  //

  int dim = 0;
  CToken t;

  if(_t == NULL)
    Consume(tIdent, &t);
  else
    t = *_t;

  if( (s->GetSymbolTable()->FindSymbol(t.GetValue())) == NULL)
    SetError(t, "a token is not in symbol table.");

  EToken tt = _scanner->Peek().GetType();
  CAstExpression *expr = NULL;
  CAstArrayDesignator *tarray = new CAstArrayDesignator(t, 
                                                        s->GetSymbolTable()->FindSymbol(t.GetValue()));

  while(tt == tLBrak){
    // { "[" expression "]" }
    Consume(tLBrak);
    expr = expression(s);
    Consume(tRBrak);
    tarray -> AddIndex( expr );
    dim++;
    tt = _scanner->Peek().GetType();
  }

  if(dim!=0){
    tarray->IndicesComplete();
    return tarray;
  }
  else{
    delete tarray;
    return new CAstDesignator(t, 
                              s->GetSymbolTable()->FindSymbol(t.GetValue()));
  }

}


CAstConstant* CParser::number(void)
{
  //
  // number ::= digit { digit }.
  //
  // "digit { digit }" is scanned as one token (tNumber)
  //

  CToken t;

  Consume(tNumber, &t);

  errno = 0;
  long long v = strtoll(t.GetValue().c_str(), NULL, 10);
  if (errno != 0) SetError(t, "invalid number.");

  return new CAstConstant(t, CTypeManager::Get()->GetInt(), v);
}


CAstConstant* CParser::boolean(void)
{
  //
  // boolean ::= "true" | "false"
  //
  // "boolean" is scanned as one token (tBoolConst)
  //

  CToken t;

  Consume(tBoolConst, &t);
  bool value = (t.GetValue() == "true");

  return new CAstConstant(t, CTypeManager::Get()->GetBool(), value);
}

CAstConstant* CParser::character(void)
{
  //
  // char ::= "'" character "'".
  // " "'" character "'" " is scanned as one token (tCharConst)
  //

  CToken t;
  Consume(tCharConst, &t);
  char value = CToken::unescape(t.GetValue()).c_str()[0];
  return new CAstConstant(t, CTypeManager::Get()->GetChar(), value);
}


CAstStringConstant* CParser::_string(CAstScope *s)
{
  //
  // string ::= '"' {character}  '"'.
  //
  // " '"' {character} '"' " is scanned as one token (tString)
  //

  CToken t;
  Consume(tString, &t);
  return new CAstStringConstant(t, t.GetValue(), s);
}


