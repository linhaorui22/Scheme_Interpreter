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
    return Expr (new RationalNum(numerator,denominator));
    //TODO: complete the rational parser
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
    //std::cout<<dynamic_cast<SymbolSyntax*>(stxs[0].get())->s<<std::endl;
    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {
        vector<Expr> parameters;
        int l_stxs=stxs.size();
        for(int i=0;i<l_stxs;i++){
            parameters.push_back(stxs[i]->parse(env));
        }
        Expr function = parameters[0]; // 提取函数类型
        parameters.erase(parameters.begin());//只保留数据
        return Expr(new Apply(function,parameters));
        //TODO: TO COMPLETE THE LOGIC
    }else{
    string op = id->s;
    //std::cout<<op<<std::endl;
    //std::cout<<op.length()<<std::endl;
    //std::cout<<primitives.count(op)<<std::endl;
    if (find(op, env).get() != nullptr) {
        vector<Expr> parameters;
        int l_stxs=stxs.size();
        for(int i=1;i<l_stxs;i++){
            parameters.push_back(stxs[i]->parse(env));
        }
        //std::cout<<1<<std::endl;
        return Expr(new Apply(Expr(new Var(op)),parameters));
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
    }
    if (primitives.count(op) != 0) {
        vector<Expr> parameters;
        int l_stxs=stxs.size();
        //std::cout<<3<<std::endl;
        //std::cout<<l_stxs<<std::endl;
        //std::cout<<stxs[1]->parse(env;)
        for(int i=1;i<l_stxs;i++){
            parameters.push_back(stxs[i]->parse(env));
        }
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        //std::cout<<4<<std::endl;
        ExprType op_type = primitives[op];
        
        if (op_type == E_PLUS) {
            if (parameters.size() == 2) {
                return Expr(new Plus(parameters[0], parameters[1])); 
            }
            else {
                if(parameters.size() >=3){
                    return Expr(new PlusVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for +");
                }
            }
        } else if (op_type == E_MINUS) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new Minus(parameters[0],parameters[1]));
            }
            else {
                if(parameters.size() >= 3){
                    return Expr(new MinusVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for -");
                }
            }
        } else if (op_type == E_MUL) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new Mult(parameters[0],parameters[1]));
            }
            else {
                if(parameters.size() >=3){
                    return Expr(new MultVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for *");
                }
            }
        }  else if (op_type == E_DIV) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new Div(parameters[0],parameters[1]));
            } 
            else {
                if(parameters.size() >=3){
                    return Expr(new DivVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for /");
                }
            }
        } else if (op_type == E_MODULO) {
            if (parameters.size() != 2) {
                throw RuntimeError("Wrong number of arguments for modulo");
            }
            return Expr(new Modulo(parameters[0], parameters[1]));
        } else if (op_type == E_LIST) {
            return Expr(new ListFunc(parameters));
        } else if (op_type == E_LT) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new Less(parameters[0],parameters[1]));
            } 
            else {
                if(parameters.size() >= 3){
                    return Expr(new LessVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for <");
                }
            }
        } else if (op_type == E_LE) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new LessEq(parameters[0],parameters[1]));
            } 
            else {
                if(parameters.size() >=3){
                    return Expr(new LessEqVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments of <=");
                }
            }
        } else if (op_type == E_EQ) {
            if(parameters.size() == 2){  
                return Expr(new Equal(parameters[0],parameters[1])); 
            }
            else {
                if(parameters.size() >=3){
                    return Expr(new EqualVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for =");// or ==
                }
            }
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_GE) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 2){
                return Expr(new GreaterEq(parameters[0],parameters[1]));
            } 
            else {
                if(parameters.size() >= 3){
                    return Expr(new GreaterEqVar(parameters));
                }
                else {
                    throw RuntimeError("Wrong number of agruments for >=");
                }
            }
        } else if (op_type == E_GT) {
            //TODO: TO COMPLETE THE LOGIC
            if(parameters.size() == 2){
                return Expr(new Greater(parameters[0],parameters[1]));
            }
            else {
                if(parameters.size() >=3){
                    return Expr(new GreaterVar(parameters));
                }
                else {
                    throw RuntimeError("wrong number of agruments for >");
                }
            }
        } else if (op_type == E_AND) {
            return Expr(new AndVar(parameters));
        } else if (op_type == E_OR) {
            return Expr(new OrVar(parameters));
        } else if (op_type == E_NOT){
            if(parameters.size() == 1){
                return Expr(new Not(parameters[0]));
            }
            else {
                throw RuntimeError("Wrong number of agruments for !");
            }
        } else if(op_type == E_EQQ){
            if(parameters.size() != 2){
                throw RuntimeError("Wrong number of agruments of =");
            }
            else {
                return Expr(new IsEq(parameters[0],parameters[1]));
            }
        } else if(op_type == E_BOOLQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for boolean");
            }
            else {
                return Expr(new IsBoolean(parameters[0]));
            }
        } else if(op_type == E_INTQ){
            if(parameters.size() !=1 ){
                throw RuntimeError("Wrong number of agruments foe integer");
            }
            else {
                return Expr(new IsFixnum(parameters[0]));
            }
        } else if(op_type == E_NULLQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for null");
            }
            else {
                return Expr(new IsNull(parameters[0]));
            }
        } else if(op_type == E_PAIRQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for pair");
            }
            else {
                return Expr(new IsPair(parameters[0]));
            }
        } else if(op_type == E_PROCQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for procedure");
            }
            else {
                return Expr(new IsProcedure(parameters[0]));
            }
        } else if(op_type == E_SYMBOLQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for symbal");
            }
            else {
                return Expr(new IsSymbol(parameters[0]));
            }
        } else if(op_type == E_LISTQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for list");
            }
            else {
                return Expr(new IsList(parameters[0]));
            }
        } else if(op_type == E_STRINGQ){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for string");
            }
            else {
                return Expr(new IsString(parameters[0]));
            }
        } else if(op_type == E_CONS){
            if(parameters.size() != 2 ){
                throw RuntimeError("Wrong number of agruments for cons");
            }
            else {
                return Expr(new Cons(parameters[0],parameters[1]));
            }
        } else if(op_type == E_CAR){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for car");
            }
            else {
                return Expr(new Car(parameters[0]));
            }
        } else if(op_type == E_CDR){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for cdr");
            }
            else {
                return Expr(new Cdr(parameters[0]));
            }
        } else if(op_type == E_SETCAR){
            if(parameters.size() != 2){
                throw RuntimeError("Wrong number of agruments for setcar");
            }
            else {
                return Expr(new SetCar(parameters[0],parameters[1]));
            }
        } else if(op_type == E_SETCDR){
            if(parameters.size() != 2){
                throw RuntimeError("Wrong number of agruments for setcdr");
            }
            else {
                return Expr(new SetCdr(parameters[0],parameters[1]));
            }
        } else if(op_type == E_DISPLAY){
            if(parameters.size() != 1){
                throw RuntimeError("Wrong number of agruments for display");
            }
            else {
                return Expr(new Display(parameters[0]));
            }
        } else if(op_type == E_EXIT){
            if(!parameters.empty()){
                throw RuntimeError("Wrong number of agruments for exit");
            }
            else {
                return Expr(new Exit());
            }
        } else if(op_type == E_VOID){
            if(!parameters.empty()){
                throw RuntimeError("Wrong number of agruments for void");
            }
            else {
                return Expr(new MakeVoid());
            }
        }
    }

    if (reserved_words.count(op) != 0) {
    	switch (reserved_words[op]) {
			//TODO: TO COMPLETE THE reserve_words PARSER LOGIC
            case E_BEGIN :{
                vector<Expr> expression;
                int l_stxs=stxs.size();
                for(int i=1;i<l_stxs;i++){
                    expression.push_back(stxs[i]->parse(env));
                }
                return Expr(new Begin(expression));
                break;
            }
            case E_QUOTE :{
                int l_stxs=stxs.size();
                if(l_stxs != 2){
                    throw RuntimeError("Wrong number of agruments for quote");
                }
                else {
                    return Expr(new Quote(stxs[1]));
                }
                break;
            }
            case E_IF :{
                int l_stxs=stxs.size();
                if(l_stxs != 4){
                    throw RuntimeError("Wrong umber of agruments for if");
                } 
                else {
                    return Expr(new If(stxs[1]->parse(env),stxs[2]->parse(env),stxs[3]->parse(env)));
                }
                break;
            }
            case E_COND :{
                //有点复杂 还没写
                vector<vector<Expr>> clauses;
                int l_stxs=stxs.size();
                for(int i = 1;i < l_stxs ; i++){
                    List* list = dynamic_cast<List*>(stxs[i].get());
                    if(!list){
                    }
                    else {
                        vector<Expr> clauses_expr;
                        for(auto& expr : list->stxs){
                            clauses_expr.push_back(expr.parse(env));
                        }
                        clauses.push_back(clauses_expr);
                    }
                }
                return Expr(new Cond(clauses));
                break;
            }
            case E_LAMBDA :{
                int l_stxs = stxs.size();
                if(l_stxs < 3){
                    throw RuntimeError("");
                }
                else {
                    List* list = dynamic_cast<List*>(stxs[1].get());
                    if(!list){
                        throw RuntimeError("");
                    }
                    else {
                        vector<string> parameters;
                        for(auto& stx : list->stxs){
                            SymbolSyntax* symbol = dynamic_cast<SymbolSyntax*>(stx.get());
                            if(!symbol){
                                throw RuntimeError("");
                            }
                            else {
                                parameters.push_back(symbol->s);
                            }
                        }
                        return Expr(new Lambda(parameters,stxs[2].parse(env)));
                    }
                }
                break;
            }
            case E_DEFINE :{
                int l_stxs = stxs.size();
                if(l_stxs < 3){
                    throw RuntimeError("");
                }
                else {
                    string define_str;
                    SymbolSyntax* symbol = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                    if(symbol){
                        return Expr(new Define(symbol->s,stxs[2].parse(env)));
                    }
                    List* list = dynamic_cast<List*>(stxs[1].get());
                    if(list && list->stxs.empty()){
                        SymbolSyntax* define_name = dynamic_cast<SymbolSyntax*>(stxs[0].get());
                        if(!define_name){
                            throw RuntimeError("");
                        }
                        vector<string> parameters;
                        int l = list->stxs.size();
                        for(int i = 1;i < l; i++ ){
                            SymbolSyntax* para = dynamic_cast<SymbolSyntax*>(list->stxs[i].get());
                            if(!para){
                                throw RuntimeError("");
                            }
                            parameters.push_back(para->s);
                        }
                        return Expr(new Define(define_name->s,Expr(new Lambda(parameters,stxs[2].parse(env)))));
                    }
                }
                break;
            }
            case E_LET :{
                int l_stxs = stxs.size();
                if(l_stxs < 3){
                    throw RuntimeError("Wrong number of agruments for let");
                }
                List* list = dynamic_cast<List*>(stxs[1].get());
                if(!list){
                    throw RuntimeError("");
                }
                vector<pair<string,Expr>> pair_let;
                for(auto& let_stx : list->stxs){
                    List* p = dynamic_cast<List*>(let_stx.get());
                    if(!p || p->stxs.size() != 2){
                        throw RuntimeError("");
                    }
                    SymbolSyntax* symbol = dynamic_cast<SymbolSyntax*>(p->stxs[0].get());//pi
                    if(!symbol){
                        throw RuntimeError("");
                    }
                    pair<string,Expr> pp = {symbol->s,p->stxs[1].parse(env)};
                    pair_let.push_back(pp);
                }
                return Expr(new Let(pair_let,stxs[2].parse(env)));
                break;
            }
            case E_LETREC:{
                int l_stxs = stxs.size();
                if(l_stxs < 3){
                    throw RuntimeError("Wrong number of agruments for let");
                }
                List* list = dynamic_cast<List*>(stxs[1].get());
                if(!list){
                    throw RuntimeError("");
                }
                vector<pair<string,Expr>> pair_letrec;
                for(auto& let_stx : list->stxs){
                    List* p = dynamic_cast<List*>(let_stx.get());
                    if(!p || p->stxs.size() != 2){
                        throw RuntimeError("");
                    }
                    SymbolSyntax* symbol = dynamic_cast<SymbolSyntax*>(p->stxs[0].get());//pi
                    if(!symbol){
                        throw RuntimeError("");
                    }
                    pair<string,Expr> pp = {symbol->s,p->stxs[1].parse(env)};
                    pair_letrec.push_back(pp);
                }
                return Expr(new Letrec(pair_letrec,stxs[2].parse(env)));
                break;
            }
            case E_SET:{
                int l_stxs = stxs.size();
                if(l_stxs != 3){
                    throw RuntimeError("Wrong numver of agruments for set");
                } 
                SymbolSyntax* symbol = dynamic_cast<SymbolSyntax*>(stxs[1].get());
                if(!symbol){
                    throw RuntimeError("");
                }
                return Expr(new Set(symbol->s,stxs[2]->parse(env)));
                break;
            }
            // lambda define let letrec set  都还没写
        	default:
            	throw RuntimeError("Unknown reserved word: " + op);
    	}
    }

    //default: use Apply to be an expression
    //TODO: TO COMPLETE THE PARSER LOGIC
    vector<Expr> paramesters;
    int l_stxs = stxs.size();
    for(int i = 1;i< l_stxs ;i++){
        paramesters.push_back(stxs[i]->parse(env));
    }
    return Expr(new Apply(Expr(new Var(op)),paramesters));
    }
}
