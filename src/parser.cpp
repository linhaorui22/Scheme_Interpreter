/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion
 * 
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {
    //TODO: complete the rational parser
    return Expr(new RationalNum(numerator,denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }

    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {
        //TODO: TO COMPLETE THE LOGIC
        Expr function=stxs[0]->parse(env);
        vector<Expr> args;
        for(int i=1;i<stxs.size();i++){
            args.push_back(stxs[i]->parse(env));
        }
        return Expr(new Apply(function,args));
    }else{
        string op = id->s;
        if (find(op, env).get() != nullptr) {
            //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
            vector<Expr>args;
            for(int i=1;i<stxs.size();i++){
                args.push_back(stxs[i]->parse(env));
            }
            return Expr(new Apply(Expr(new Var(op)),args));
        }
        if (primitives.count(op) != 0) {
            vector<Expr> parameters;
            for(int i=1;i<stxs.size();i++)
                parameters.push_back(stxs[i]->parse(env));
            //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
            ExprType op_type = primitives[op];
            if (op_type == E_PLUS) {
                if (parameters.size() == 2) {
                    return Expr(new Plus(parameters[0], parameters[1])); 
                } else {
                    return Expr(new PlusVar(parameters));
                }
            } else if (op_type == E_MINUS) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Minus(parameters[0], parameters[1])); 
                } else {
                    return Expr(new MinusVar(parameters));
                }
            } else if (op_type == E_MUL) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Mult(parameters[0], parameters[1])); 
                } else {
                    return Expr(new MultVar(parameters));
                }
            }  else if (op_type == E_DIV) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() == 2) {
                    return Expr(new Div(parameters[0], parameters[1])); 
                } else {
                    return Expr(new DivVar(parameters));
                }
            } else if (op_type == E_MODULO) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of arguments for modulo");
                }
                return Expr(new Modulo(parameters[0], parameters[1]));
            } else if (op_type == E_EXPT) {
                if (parameters.size() == 2) {
                    return Expr(new Expt(parameters[0], parameters[1]));
                } else {
                    throw RuntimeError("Wrong number of arguments for expt");
                }
            } else if (op_type == E_LIST) {
                return Expr(new ListFunc(parameters));
            } else if (op_type == E_LT) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <");
                if (parameters.size() == 2) {
                    return Expr(new Less(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessVar(parameters));
                }
            } else if (op_type == E_LE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for <=");
                if (parameters.size() == 2) {
                    return Expr(new LessEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new LessEqVar(parameters));
                }
            } else if (op_type == E_EQ) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for =");
                if (parameters.size() == 2) {
                    return Expr(new Equal(parameters[0], parameters[1])); 
                } else {
                    return Expr(new EqualVar(parameters));
                }
            } else if (op_type == E_GE) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >=");
                if (parameters.size() == 2) {
                    return Expr(new GreaterEq(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterEqVar(parameters));
                }
            } else if (op_type == E_GT) {
                //TODO: TO COMPLETE THE LOGIC
                if (parameters.size() <= 1) throw RuntimeError("Wrong number of arguments for >");
                if (parameters.size() == 2) {
                    return Expr(new Greater(parameters[0], parameters[1])); 
                } else {
                    return Expr(new GreaterVar(parameters));
                }
                return Expr(new GreaterVar(parameters));
            } else if (op_type == E_AND) {
                return Expr(new AndVar(parameters));
            } else if (op_type == E_OR) {
                return Expr(new OrVar(parameters));
            } else if (op_type == E_NOT) {
                if(parameters.size()!=1)
                    throw RuntimeError("Wrong number of not");
                return Expr(new Not(parameters[0]));
            }else if (op_type == E_CAR) {
                if (parameters.size() == 1) {
                    return Expr(new Car(parameters[0]));
                } else {
                    throw RuntimeError("Wrong number of arguments for car");
                }
            } else if (op_type == E_CDR) {
                if (parameters.size() == 1) {
                    return Expr(new Cdr(parameters[0]));
                } else {
                    throw RuntimeError("Wrong number of arguments for cdr");
                }
            } else if (op_type == E_CONS) {
                if (parameters.size() == 2) {
                    return Expr(new Cons(parameters[0], parameters[1]));
                } else {
                    throw RuntimeError("Wrong number of arguments for cons");
                }
        
            } else if (op_type == E_SETCAR) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of set-car!");
                }
                return Expr(new SetCar(parameters[0], parameters[1]));
            } else if (op_type == E_SETCDR) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of set-cdr!");
                }
                return Expr(new SetCdr(parameters[0], parameters[1]));
            } else if (op_type == E_LIST) {
                return Expr(new ListFunc(parameters));
            } else if (op_type == E_LISTQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of list?");
                }
                return Expr(new IsList(parameters[0]));
            } else if (op_type == E_BOOLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of boolean?");
                }
                return Expr(new IsBoolean(parameters[0]));
            } else if (op_type == E_INTQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of integer?");
                }
                return Expr(new IsFixnum(parameters[0]));
            } else if (op_type == E_NULLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of null?");
                }
                return Expr(new IsNull(parameters[0]));
            } else if (op_type == E_PAIRQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of pair?");
                }
                return Expr(new IsPair(parameters[0]));
            } else if (op_type == E_PROCQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of procedure?");
                }
                return Expr(new IsProcedure(parameters[0]));
            } else if (op_type == E_SYMBOLQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of symbol?");
                }
                return Expr(new IsSymbol(parameters[0]));
            } else if (op_type == E_STRINGQ) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of string?");
                }
                return Expr(new IsString(parameters[0]));
            } else if (op_type == E_EQQ) {
                if (parameters.size() != 2) {
                    throw RuntimeError("Wrong number of eq?");
                }
                return Expr(new IsEq(parameters[0], parameters[1]));
            } else if (op_type == E_DISPLAY) {
                if (parameters.size() != 1) {
                    throw RuntimeError("Wrong number of display");
                }
                return Expr(new Display(parameters[0]));
            } else if (op_type == E_VOID) {
                if (!parameters.empty()) {
                    throw RuntimeError("Wrong number of void");
                }
                return Expr(new MakeVoid());
            } else if (op_type == E_EXIT) {
                if (!parameters.empty()) {
                    throw RuntimeError("Wrong number of exit");
                }
                return Expr(new Exit());
            } else {
                //TODO: TO COMPLETE THE LOGIC
                throw RuntimeError("Unknown primitives: "+op);
            } 
        }
        if (reserved_words.count(op) != 0) {
            switch (reserved_words[op]) {
                //TODO: TO COMPLETE THE reserve_words PARSER LOGIC
                case E_BEGIN:{
                    vector<Expr> exprs;
                    for(int i=1;i<stxs.size();i++){
                        exprs.push_back(stxs[i]->parse(env));
                    }
                    return Expr(new Begin(exprs));
                }
                case E_IF:{
                    if(stxs.size()!=4){
                        throw RuntimeError("");
                    }
                    return Expr(new If(stxs[1]->parse(env),stxs[2]->parse(env),stxs[3]->parse(env)));//if表达式
                }
                case E_LAMBDA: {
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for lambda");
                    }
                    List *list = dynamic_cast<List*>(stxs[1].get());
                    if (!list) {
                        throw RuntimeError("");
                    }
                    vector<string> parms;
                    for(auto &parm : list->stxs) {
                        SymbolSyntax *sym = dynamic_cast<SymbolSyntax*>(parm.get());
                        if (!sym) {
                            throw RuntimeError("");
                        }
                        parms.push_back(sym->s);
                    }
                    Assoc new_env = env;
                    for (const auto &param : parms) {
                        new_env = extend(param, VoidV(), new_env);//用空来在新环境占位
                    }
                    if (stxs.size() > 3) {
                        vector<Expr> expr;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            expr.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Lambda(parms, Expr(new Begin(expr))));
                    } else {
                        return Expr(new Lambda(parms, stxs[2]->parse(new_env)));
                    }
                }    
                case E_QUOTE:{
                    if(stxs.size()==2)return Expr(new Quote(stxs[1]));
                    else {
                        throw RuntimeError("Wrong number of arguments for quote");
                    }
                }
                case E_DEFINE: {
                    if (stxs.size() < 3) {
                        throw RuntimeError("Wrong number of agruments for define");
                    }
                    //define (name expr )body
                    //先处理定义
                    List* list = dynamic_cast<List*>(stxs[1].get());   
                    if (list != nullptr) {//如果是函数
                        if (list->stxs.empty()) {
                            throw RuntimeError("");
                        }
                        SymbolSyntax* name = dynamic_cast<SymbolSyntax*>(list->stxs[0].get());
                        if (name == nullptr) {
                            throw RuntimeError("");
                        }

                        vector<string> params;//定义细节  参数&符号
                        for (size_t i = 1 ; i < list->stxs.size() ; i ++) {
                            SymbolSyntax* sym = dynamic_cast<SymbolSyntax*>(list->stxs[i].get());
                            if (sym == nullptr) {
                                throw RuntimeError("");
                            }
                            params.push_back(sym->s);
                        }

                        Assoc new_env = env;
                        for (const auto& name : params) {
                            new_env = extend(name, VoidV(), new_env);//新环境
                        }
                        vector<Expr> expr;
                        for (size_t i = 2; i < stxs.size(); i++) {
                            expr.push_back(stxs[i]->parse(new_env));
                        }
                        if (expr.empty()) {
                            return Expr(new Define(name->s, Expr(new Lambda(params, Expr(new MakeVoid())))));
                        } else if (expr.size() == 1) {
                            return Expr(new Define(name->s, Expr(new Lambda(params, expr[0]))));
                        } else {
                            return Expr(new Define(name->s, Expr(new Lambda(params, Expr(new Begin(expr))))));
                        }
                    } else {
                        SymbolSyntax* name = dynamic_cast<SymbolSyntax*>(stxs[1].get());//如果是变量
                        if (name == nullptr) {
                            throw RuntimeError("");
                        }
                        return Expr(new Define(name->s, stxs[2]->parse(env)));
                    }
                }
                case E_COND:{
                    if(stxs.size()<=1){
                        throw RuntimeError("");
                    }
                    //clause 对应 相应的 test
                    vector<vector<Expr>> clauses;
                    for(int i = 1; i < stxs.size(); i++) {
                        List *clause = dynamic_cast<List*>(stxs[i].get());
                        if(!clause){
                            throw RuntimeError("");
                        }
                        if(clause->stxs.empty()){
                            throw RuntimeError("");
                        }
                        vector<Expr> clause_exprs;
                        for(auto &expr : clause->stxs){
                            clause_exprs.push_back(expr->parse(env));                        
                        }
                        clauses.push_back(clause_exprs);
                    }
                    return Expr(new Cond(clauses));
                }
                case E_LET:{
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for let");
                    }
                    List *list = dynamic_cast<List*>(stxs[1].get());
                    if (!list) {
                        throw RuntimeError("");
                    }
                    Assoc new_env = env;
                    vector<pair<string, Expr>> bindings;
                    for(auto &binding : list->stxs) {
                        //binding 是 var & expr
                        List *pair = dynamic_cast<List*>(binding.get());
                        if (!pair) {
                            throw RuntimeError("");
                        }
                        if(pair->stxs.size()!= 2){
                            throw RuntimeError("");
                        }
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(pair->stxs[0].get());
                        if (!var) {
                            throw RuntimeError("");
                        }
                        bindings.push_back({var->s,pair->stxs[1]->parse(env)});
                        new_env = extend(var->s, VoidV(), new_env);//先进行新环境的创建
                    }
                    if (stxs.size() > 3) {//多个表达式
                        vector<Expr> exprs;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            exprs.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Let(bindings, Expr(new Begin(exprs))));
                    } else {
                        return Expr(new Let(bindings, stxs[2]->parse(new_env)));
                    }
                }
                case E_LETREC:{
                    if(stxs.size() < 3) {
                        throw RuntimeError("Wrong number of arguments for letrec");
                    }
                    List *list = dynamic_cast<List*>(stxs[1].get());
                    if (!list) {
                        throw RuntimeError("");
                    }
                    Assoc new_env = env;
                    vector<pair<string, Expr>> bindings;
                    for(auto &binding : list->stxs) {
                        List *pair = dynamic_cast<List*>(binding.get());
                        if (!pair) {
                            throw RuntimeError("");
                        }
                        if(pair->stxs.size()!= 2){
                            throw RuntimeError("");
                        }
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(pair->stxs[0].get());
                        if (!var) {
                            throw RuntimeError("");
                        }
                        new_env = extend(var->s, VoidV(), new_env);
                    }
                    //与let的区别所在，可以相互用
                    for(auto &binding : list->stxs) {
                        List *pair = dynamic_cast<List*>(binding.get());
                        SymbolSyntax *var = dynamic_cast<SymbolSyntax*>(pair->stxs[0].get());
                        bindings.push_back({var->s, pair->stxs[1]->parse(new_env)});
                    }
                    if (stxs.size() > 3) {
                        vector<Expr> exprs;
                        for(size_t i = 2; i < stxs.size(); i++) {
                            exprs.push_back(stxs[i]->parse(new_env));
                        }
                        return Expr(new Letrec(bindings, Expr(new Begin(exprs))));
                    } else {
                        return Expr(new Letrec(bindings, stxs[2]->parse(new_env)));
                    }
                }
                case E_SET:{
                    if(stxs.size() != 3) {
                            throw RuntimeError("Wrong number of arguments for set!");
                        }
                        SymbolSyntax *name = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                        if (!name) {
                            throw RuntimeError("");
                        }
                        return Expr(new Set(name->s, stxs[2]->parse(env)));
                }
                default:
                    throw RuntimeError("Unknown reserved word: " + op);
            }
        }
  
        //default: use Apply to be an expression
        //TODO: TO COMPLETE THE PARSER LOGIC
        Expr rator=stxs[0]->parse(env);
        vector<Expr>args;
        for(int i=1;i<stxs.size();i++)
            args.push_back(stxs[i]->parse(env));
        return Expr(new Apply(rator,args));
    }
}