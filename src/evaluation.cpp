/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp" 
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>
#include <vector>
int gcd1(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    // TODO: TO COMPLETE THE VARIADIC CLASS
    std::vector<Value> Args;
    for(auto & arg : rands){
        Args.push_back(arg->eval(e));
    } 
    return evalRator(Args);
}

Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    
    Value matched_value = find(x, e);
    if (matched_value.get() == nullptr) {
        if (primitives.count(x)) {
             static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                    {E_VOID,     {new MakeVoid(), {}}},
                    {E_EXIT,     {new Exit(), {}}},
                    {E_BOOLQ,    {new IsBoolean(new Var("parm")), {"parm"}}},
                    {E_INTQ,     {new IsFixnum(new Var("parm")), {"parm"}}},
                    {E_NULLQ,    {new IsNull(new Var("parm")), {"parm"}}},
                    {E_PAIRQ,    {new IsPair(new Var("parm")), {"parm"}}},
                    {E_PROCQ,    {new IsProcedure(new Var("parm")), {"parm"}}},
                    {E_SYMBOLQ,  {new IsSymbol(new Var("parm")), {"parm"}}},
                    {E_STRINGQ,  {new IsString(new Var("parm")), {"parm"}}},
                    {E_DISPLAY,  {new Display(new Var("parm")), {"parm"}}},
                    {E_PLUS,     {new PlusVar({}),  {}}},
                    {E_MINUS,    {new MinusVar({}), {}}},
                    {E_MUL,      {new MultVar({}),  {}}},
                    {E_DIV,      {new DivVar({}),   {}}},
                    {E_MODULO,   {new Modulo(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EXPT,     {new Expt(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_EQQ,      {new EqualVar({}), {}}},
            };

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
                return ProcedureV(it->second.second,it->second.first,e);
            }
      }
    }
    return matched_value;
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //TODO: To complete the addition logic
    if(rand1->v_type == V_INT && rand2->v_type == V_INT ){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(n1+n2);
    } else if((rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) || (rand2->v_type == V_RATIONAL && rand1->v_type == V_INT)|| (rand2->v_type == V_RATIONAL &&rand1->v_type == V_RATIONAL )) {
        int num1,num2,den1,den2;
        if(rand1->v_type == V_INT){
            num1 = dynamic_cast<Integer*>(rand1.get())->n;
            den1 = 1;
        } else{
            num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
            den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
        }
        if(rand2->v_type == V_INT){
            num2 = dynamic_cast<Integer*>(rand2.get())->n;
            den2 = 1;
        } else{
            num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
            den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
        }
        int ans_num = num1*den2+num2*den1;
        int ans_den = den1*den2;
        if(ans_num % ans_den == 0){
            return IntegerV(int(ans_num/ans_den));
        }
        else {
            return RationalV(ans_num,ans_den);
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    //TODO: To complete the substraction logic
    if(rand1->v_type == V_INT && rand2->v_type == V_INT ){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(n1-n2);
    } else if((rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) || (rand2->v_type == V_RATIONAL && rand1->v_type == V_INT)|| (rand2->v_type == V_RATIONAL &&rand1->v_type == V_RATIONAL )) {
        int num1,num2,den1,den2;
        if(rand1->v_type == V_INT){
            num1 = dynamic_cast<Integer*>(rand1.get())->n;
            den1 = 1;
        } else{
            num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
            den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
        }
        if(rand2->v_type == V_INT){
            num2 = dynamic_cast<Integer*>(rand2.get())->n;
            den2 = 1;
        } else{
            num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
            den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
        }
        int ans_num = num1*den2-num2*den1;
        int ans_den = den1*den2;
        if(ans_num % ans_den == 0){
            return IntegerV(int(ans_num/ans_den));
        }
        else {
            return RationalV(ans_num,ans_den);
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    //TODO: To complete the Multiplication logic
    if(rand1->v_type == V_INT && rand2->v_type == V_INT ){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        return IntegerV(n1*n2);
    } else if((rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) || (rand2->v_type == V_RATIONAL && rand1->v_type == V_INT)|| (rand2->v_type == V_RATIONAL &&rand1->v_type == V_RATIONAL )) {
        int num1,num2,den1,den2;
        if(rand1->v_type == V_INT){
            num1 = dynamic_cast<Integer*>(rand1.get())->n;
            den1 = 1;
        } else{
            num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
            den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
        }
        if(rand2->v_type == V_INT){
            num2 = dynamic_cast<Integer*>(rand2.get())->n;
            den2 = 1;
        } else{
            num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
            den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
        }
        int ans_num = num1*num2;
        int ans_den = den1*den2;
        if(ans_num % ans_den == 0){
            return IntegerV(int(ans_num/ans_den));
        }
        else {
            return RationalV(ans_num,ans_den);
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    //TODO: To complete the dicision logic
    if(rand1->v_type == V_INT && rand2->v_type == V_INT ){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        if (n2 == 0){
            throw RuntimeError("Division by zero");
        }
        if (n1 % n2 ==0){
            return IntegerV(n1/n2);
        } else {
            return RationalV(n1,n2);
        }
    } else if((rand1->v_type == V_RATIONAL && rand2->v_type == V_INT) || (rand2->v_type == V_RATIONAL && rand1->v_type == V_INT)|| (rand2->v_type == V_RATIONAL &&rand1->v_type == V_RATIONAL )) {
        int num1,num2,den1,den2;
        if(rand1->v_type == V_INT){
            num1 = dynamic_cast<Integer*>(rand1.get())->n;
            den1 = 1;
        } else{
            num1 = dynamic_cast<Rational*>(rand1.get())->numerator;
            den1 = dynamic_cast<Rational*>(rand1.get())->denominator;
        }
        if(rand2->v_type == V_INT){
            num2 = dynamic_cast<Integer*>(rand2.get())->n;
            den2 = 1;
        } else{
            num2 = dynamic_cast<Rational*>(rand2.get())->numerator;
            den2 = dynamic_cast<Rational*>(rand2.get())->denominator;
        }
        int ans_num = num1*den2;
        int ans_den = den1*num2;
        if(ans_den == 0){
            //throw RuntimeError("Division by zero");
        }
        if(ans_num % ans_den ==0){
            return IntegerV(ans_num/ans_den);
        } else {
            return RationalV(ans_num,ans_den);
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    //TODO: To complete the addition logic
    if(args.size() == 0){
        return IntegerV(0);
    }
    else {
        Value ans = args[0];
        int l_args = args.size();
        int ans_num,ans_den;
        if(ans->v_type == V_INT){
            ans_num = dynamic_cast<Integer*>(ans.get())->n;
            ans_den = 1;
        } else if(ans->v_type == V_RATIONAL){
            ans_num = dynamic_cast<Rational*>(ans.get())->numerator;
            ans_den = dynamic_cast<Rational*>(ans.get())->denominator;
        } else {
            //throw RuntimeError("Wrong tupename");
        }
        int arg_num =1,arg_den =1;
        for(int i=1;i<l_args;i++){
            if(args[i]->v_type == V_INT){
                arg_num = dynamic_cast<Integer*>(args[i].get())->n;
                arg_den = 1;
            } else if(args[i]->v_type == V_RATIONAL){
                arg_num = dynamic_cast<Rational*>(args[i].get())->numerator;
                arg_den = dynamic_cast<Rational*>(args[i].get())->denominator;
            } else {
            throw RuntimeError("Wrong tupename");
            } // 处理加进来的数
            ans_num = ans_num*arg_den + ans_den*arg_num;
            ans_den = ans_den*arg_den;
            int g = gcd1(ans_num,ans_den);
            ans_den /= g;
            ans_num /= g;
        }
        if(ans_num % ans_den == 0){
            return IntegerV(ans_num/ans_den);
        } else {
            return RationalV(ans_num,ans_den);
        }
    }
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    //TODO: To complete the substraction logic
    if(args.size() == 0){
        return IntegerV(0);
    }
    else {
        Value ans = args[0];
        int l_args = args.size();
        int ans_num,ans_den;
        if(ans->v_type == V_INT){
            ans_num = dynamic_cast<Integer*>(ans.get())->n;
            ans_den = 1;
        } else if(ans->v_type == V_RATIONAL){
            ans_num = dynamic_cast<Rational*>(ans.get())->numerator;
            ans_den = dynamic_cast<Rational*>(ans.get())->denominator;
        } else {
            throw RuntimeError("Wrong tupename");
        }
        int arg_num = 1,arg_den = 1;
        for(int i=1;i<l_args;i++){
            if(args[i]->v_type == V_INT){
                arg_num = dynamic_cast<Integer*>(args[i].get())->n;
                arg_den = 1;
            } else if(args[i]->v_type == V_RATIONAL){
                arg_num = dynamic_cast<Rational*>(args[i].get())->numerator;
                arg_den = dynamic_cast<Rational*>(args[i].get())->denominator;
            } else {
            throw RuntimeError("Wrong tupename");
            } // 处理减进来的数
            ans_num = ans_num*arg_den - ans_den*arg_num;
            ans_den = ans_den*arg_den;
            int g = gcd1(ans_num,ans_den);
            ans_den /= g;
            ans_num /= g;
        }
        if(ans_num % ans_den == 0){
            return IntegerV(ans_num/ans_den);
        } else {
            return RationalV(ans_num,ans_den);
        }
    }
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    //TODO: To complete the multiplication logic
    if(args.size() == 0){
        return IntegerV(0);
    }
    else {
        Value ans = args[0];
        int l_args = args.size();
        int ans_num,ans_den;
        if(ans->v_type == V_INT){
            ans_num = dynamic_cast<Integer*>(ans.get())->n;
            ans_den = 1;
        } else if(ans->v_type == V_RATIONAL){
            ans_num = dynamic_cast<Rational*>(ans.get())->numerator;
            ans_den = dynamic_cast<Rational*>(ans.get())->denominator;
        } else {
            throw RuntimeError("Wrong tupename");
        }
        int arg_num = 1,arg_den = 1;
        for(int i=1;i<l_args;i++){
            if(args[i]->v_type == V_INT){
                arg_num = dynamic_cast<Integer*>(args[i].get())->n;
                arg_den = 1;
            } else if(args[i]->v_type == V_RATIONAL){
                arg_num = dynamic_cast<Rational*>(args[i].get())->numerator;
                arg_den = dynamic_cast<Rational*>(args[i].get())->denominator;
            } else {
            throw RuntimeError("Wrong tupename");
            } // 处理乘进来的数
            ans_num = ans_num*arg_num;
            ans_den = ans_den*arg_den;
            int g = gcd1(ans_num,ans_den);
            ans_den /= g;
            ans_num /= g;
        }
        if(ans_num % ans_den == 0){
            return IntegerV(ans_num/ans_den);
        } else {
            return RationalV(ans_num,ans_den);
        }
    }
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    //TODO: To complete the divisor logic
    if(args.size() == 0){
        return IntegerV(0);
    }
    else {
        Value ans = args[0];
        int l_args = args.size();
        int ans_num,ans_den;
        if(ans->v_type == V_INT){
            ans_num = dynamic_cast<Integer*>(ans.get())->n;
            ans_den = 1;
        } else if(ans->v_type == V_RATIONAL){
            ans_num = dynamic_cast<Rational*>(ans.get())->numerator;
            ans_den = dynamic_cast<Rational*>(ans.get())->denominator;
        } else {
            throw RuntimeError("Wrong tupename");
        }
        int arg_num = 1,arg_den = 1;
        for(int i=1;i<l_args;i++){
            if(args[i]->v_type == V_INT){
                arg_num = dynamic_cast<Integer*>(args[i].get())->n;
                arg_den = 1;
            } else if(args[i]->v_type == V_RATIONAL){
                arg_num = dynamic_cast<Rational*>(args[i].get())->numerator;
                arg_den = dynamic_cast<Rational*>(args[i].get())->denominator;
            } else {
            throw RuntimeError("Wrong tupename");
            } // 处理除进来的数
            if(arg_num == 0){
                throw RuntimeError("Division by zero");
            }
            ans_num = ans_num*arg_den;
            ans_den = ans_den*arg_num;
            int g = gcd1(ans_num,ans_den);
            ans_den /= g;
            ans_num /= g;
        }
        if(ans_num % ans_den == 0){
            return IntegerV(ans_num/ans_den);
        } else {
            return RationalV(ans_num,ans_den);
        }
    }
}

Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}
// < -> -1  > -> 1 == -> 0

bool check(const Value & rand1,const Value &rand2){
    if((rand1->v_type == V_INT && rand2->v_type == V_INT) ||(rand1->v_type == V_RATIONAL && rand2->v_type == V_INT)||(rand1->v_type == V_INT && rand2->v_type == V_RATIONAL)||(rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL)){
        return true;
    }
    else {
        return false;
    }
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //TODO: To complete the less logic
    if(check(rand1,rand2)){
        int cmp=compareNumericValues(rand1,rand2);
        return BooleanV(cmp < 0);
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //TODO: To complete the lesseq logic
    if(check(rand1,rand2)){
        int cmp=compareNumericValues(rand1,rand2);
        return BooleanV(cmp <= 0);
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    //TODO: To complete the equal logic
    if(check(rand1,rand2)){
        int cmp=compareNumericValues(rand1,rand2);
        return BooleanV(!cmp);
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //TODO: To complete the greatereq logic
    if(check(rand1,rand2)){
        int cmp=compareNumericValues(rand1,rand2);
        return BooleanV(cmp >= 0);
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //TODO: To complete the greater logic
    if(check(rand1,rand2)){
        int cmp=compareNumericValues(rand1,rand2);
        return BooleanV(cmp > 0);
    }
    else {
        throw(RuntimeError("Wrong typename"));
    }
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //TODO: To complete the less logic
    int l_args = args.size();
    if( l_args < 2){
        throw RuntimeError("Wrong ...");
    }
    else {
        for(int i = 0;i<l_args-1;i++){
            if(check(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]))){
                int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
                if( cmp != -1){
                    return BooleanV(false);
                }
            }
            else {
                throw("Wrong typename");
            }
           /* 
            int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
            if(cmp != -1){
                return BooleanV(false);
            }
            */
        }
        return BooleanV(true);
    }
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //TODO: To complete the lesseq logic
    int l_args = args.size();
    if( l_args < 2){
        throw RuntimeError("Wrong ...");
    }
    else {
        for(int i = 0;i<l_args-1;i++){
            if(check(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]))){
                int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
                if( cmp == 1){
                    return BooleanV(false);
                }
            }
            else {
                throw("Wrong typename");
            }
           /* 
            int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
            if(cmp != -1){
                return BooleanV(false);
            }
            */
        }
        return BooleanV(true);
    }
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //TODO: To complete the equal logic
    int l_args = args.size();
    if( l_args < 2){
        throw RuntimeError("Wrong ...");
    }
    else {
        for(int i = 0;i<l_args-1;i++){
            if(check(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]))){
                int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
                if( cmp != 0){
                    return BooleanV(false);
                }
            }
            else {
                throw("Wrong typename");
            }
           /* 
            int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
            if(cmp != -1){
                return BooleanV(false);
            }
            */
        }
        return BooleanV(true);
    }
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //TODO: To complete the greatereq logic
    int l_args = args.size();
    if( l_args < 2){
        throw RuntimeError("Wrong ...");
    }
    else {
        for(int i = 0;i<l_args-1;i++){
            if(check(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]))){
                int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
                if( cmp == -1){
                    return BooleanV(false);
                }
            }
            else {
                throw("Wrong typename");
            }
           /* 
            int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
            if(cmp != -1){
                return BooleanV(false);
            }
            */
        }
        return BooleanV(true);
    }
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //TODO: To complete the greater logic
    int l_args = args.size();
    if( l_args < 2){
        throw RuntimeError("Wrong ...");
    }
    else {
        for(int i = 0;i<l_args-1;i++){
            if(check(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]))){
                int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
                if( cmp != 1){
                    return BooleanV(false);
                }
            }
            else {
                throw("Wrong typename");
            }
           /* 
            int cmp = compareNumericValues(static_cast<Value> (args[i]),static_cast<Value> (args[i+1]));
            if(cmp != -1){
                return BooleanV(false);
            }
            */
        }
        return BooleanV(true);
    }
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //TODO: To complete the cons logic
    return PairV(rand1,rand2);
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //TODO: To complete the list logic
    int l_args = args.size();
    Value ans = NullV();
    for(int i=l_args-1;i>=0;i--){
        ans = PairV(static_cast<Value> (args[i]),static_cast<Value> (ans));
    }
    return ans;
}

Value IsList::evalRator(const Value &rand) { // list?
    //TODO: To complete the list? logic
    Value now = rand;
    while (rand->v_type == V_PAIR){
        now = dynamic_cast<Pair*>(now.get())->cdr;
    }
    return BooleanV(now->v_type == V_NULL);
}

Value Car::evalRator(const Value &rand) { // car
    //TODO: To complete the car logic
    if(rand->v_type != V_PAIR){
        throw RuntimeError("Wrong typename");
    }
    Pair* p = dynamic_cast<Pair*>(rand.get());
    return p->car;
}

Value Cdr::evalRator(const Value &rand) { // cdr
    //TODO: To complete the cdr logic
    if(rand->v_type != V_PAIR){
        throw RuntimeError("Wrong typename");
    }
    Pair* p = dynamic_cast<Pair*>(rand.get());
    return p->cdr;
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic
    if(rand1->v_type != V_PAIR){
        throw RuntimeError("Wrong typename");
    }
    Pair* p = dynamic_cast<Pair*>(rand1.get());
    p->car = rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic
   if(rand1->v_type != V_PAIR){
        throw RuntimeError("Wrong typename");
    }
    Pair* p = dynamic_cast<Pair*>(rand1.get());
    p->cdr = rand2;
    return VoidV();
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}



Value Begin::eval(Assoc &e) {
    //TODO: To complete the begin logic
    if(es.empty()){
        return VoidV();
    }    
    Value ans = es[0]->eval(e);
    int l_es = es.size();
    for(int i = 1;i<l_es;i++){
        ans = es[i]->eval(e);
    }
    return ans;
}

Value Quote::eval(Assoc& e) {
    //TODO: To complete the quote logic
    if(auto num_syn = dynamic_cast<Number*>(s.get())){
        return IntegerV(num_syn->n);
    }
    else {
        if(auto str_syn = dynamic_cast<SymbolSyntax*>(s.get())){
            return SymbolV(str_syn->s);
        }
        else {
            if(auto true_syn = dynamic_cast<TrueSyntax*>(s.get())){
                return BooleanV(true);
            }
            else {
                if(auto false_syn = dynamic_cast<FalseSyntax*>(s.get())){
                    return BooleanV(false);
                }
                else {
                    if(auto list_syn = dynamic_cast<List*>(s.get())){
                        if(list_syn->stxs.empty()){
                            return NullV();
                        }
                    }
                }
            }
        }
    }
}



Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
    if(rands.empty()){
        return BooleanV(true);
    }
    bool flag = true;
    int l_rands = rands.size();
    for(int i=0;i<l_rands;i++){
        if(rands[i]->eval(e)->v_type == V_BOOL && !dynamic_cast<Boolean*>(rands[i]->eval(e).get())->b){
            flag = false;
        }
    }
    return BooleanV(flag);
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
    if(rands.empty()){
        return BooleanV(true);
    }
    bool flag = false;
    int l_rands = rands.size();
    for(int i=0;i<l_rands;i++){
        if(rands[i]->eval(e)->v_type != V_BOOL || dynamic_cast<Boolean*>(rands[i]->eval(e).get())->b){
            flag = true;
        }
    }
    return BooleanV(flag);
}

Value Not::evalRator(const Value &rand) { // not
    //TODO: To complete the not logic
    if( rand->v_type == V_BOOL && !dynamic_cast<Boolean*>(rand.get())->b){
        return BooleanV(true);
    }
    else {
        return BooleanV(false);
    }
}

Value If::eval(Assoc &e) {
    //TODO: To complete the if logic
    Value ans = cond->eval(e);
    if(ans->v_type == V_BOOL && !dynamic_cast<Boolean*>(ans.get())->b){
        return alter->eval(e);
    } 
    else {
        return conseq->eval(e);
    }
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
    for( auto& clause : clauses){
        if(clause.empty()){
            continue;
        }
        if(clause.size() >= 2){
            Value ans = clause[0]->eval(env);
            if(ans->v_type != V_BOOL || dynamic_cast<Boolean*>(ans.get())->b){
                Value result = clause[1]->eval(env);
                int l_clause = clause.size();
                for(int i = 2;i< l_clause;i++){
                    result = clause[i]->eval(env);
                }
                return result;
            }
        }
    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) { 
    //TODO: To complete the lambda logic
    return ProcedureV(x,e,env);
}

Value Apply::eval(Assoc &e) {
    if (rator->eval(e)->v_type != V_PROC) {throw RuntimeError("Attempt to apply a non-procedure");}

    //TODO: TO COMPLETE THE CLOSURE LOGIC
    Procedure* clos_ptr = 0;// 还没写
    clos_ptr = dynamic_cast<Procedure*>(rator->eval(e).get());
    //TODO: TO COMPLETE THE ARGUMENT PARSER LOGIC
    std::vector<Value> args;
    if (auto varNode = dynamic_cast<Variadic*>(clos_ptr->e.get())) {
        //TODO
        args.push_back(varNode->eval(e));
    }
    if (args.size() != clos_ptr->parameters.size()) throw RuntimeError("Wrong number of arguments");
    
    //TODO: TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
    Assoc param_env = 0;// hai mei xie
    for(int i =0 ; i < clos_ptr->parameters.size() ; i++){
        param_env = extend(clos_ptr->parameters[i],args[i],param_env);
    }
    return clos_ptr->e->eval(param_env);
}

Value Define::eval(Assoc &env) {
    //TODO: To complete the define logic
    Value value = e->eval(env);
    modify(var,value,env);
    return VoidV();
}

Value Let::eval(Assoc &env) {
    //TODO: To complete the let logic
    std::vector<std::pair<std::string,Value>> let_bind;
    for(auto& binds : bind){
        Value put_bind = binds.second->eval(env);
        std::pair<std::string,Value> p = {binds.first,put_bind};
        let_bind.push_back(p);
    } // 让每个变量等于表达式的值
    Assoc new_env = env;
    for(auto& new_bind : let_bind){
        new_env = extend(new_bind.first,new_bind.second,new_env);
    }
    return body->eval(new_env);
}

Value Letrec::eval(Assoc &env) {
    //TODO: To complete the letrec logic
    Assoc new_env = env;
    for(auto& new_bind : bind ){
        new_env = extend(new_bind.first,VoidV(),new_env);
    }
    for(auto& new_bind : bind){
        Value ans = new_bind.second->eval(new_env);
        modify(new_bind.first,ans,new_env);
    }
    return body->eval(new_env);
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
    Value ans = e->eval(env);
    modify(var,ans,env);
    return VoidV();
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    
    return VoidV();
}
