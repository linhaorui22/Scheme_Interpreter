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

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;
int gcd1(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}
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
    std::vector<Value> vals;
    for(auto &r:rands)vals.push_back(r->eval(e));
    return evalRator(vals);
}


Value Var::eval(Assoc &e) { // evaluation of variable
    // TODO: TO identify the invalid variable
    // We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    //bool flag1 = true;
    /*char c = x[0];
    if(check_number(c) || c == '.' || c == '@'){
        throw RuntimeError("");
    }
    for(char c : x){
        if(c == '#' ||  c == '\'' || c == '\"' || c == '`' ){
            throw RuntimeError("");
        }
    }*/
    
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
                    {E_EQ,       {new EqualVar({}), {}}},
                    {E_LT,       {new LessVar({}), {}}},
                    {E_LE,       {new LessEqVar({}), {}}},
                    {E_GE,       {new GreaterEqVar({}), {}}},
                    {E_GT,       {new GreaterVar({}), {}}},
                    {E_CONS,     {new Cons(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_CAR,      {new Car(new Var("parm")), {"parm"}}},
                    {E_CDR,      {new Cdr(new Var("parm")), {"parm"}}},
                    {E_NOT,      {new Not(new Var("parm")), {"parm"}}},
                    {E_LIST,     {new ListFunc({}), {}}},
                    {E_LISTQ,    {new IsList(new Var("parm")), {"parm"}}},
                    {E_SETCAR,   {new SetCar(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_SETCDR,   {new SetCdr(new Var("parm1"), new Var("parm2")), {"parm1","parm2"}}},
                    {E_AND,      {new AndVar({}), {}}},
                    {E_OR,       {new OrVar({}), {}}}
            };

            auto it = primitive_map.find(primitives[x]);
            //TOD0:to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                //TODO
                return ProcedureV(it->second.second,it->second.first,e);
            }
        }
        throw(RuntimeError("Undefined "));
    }
    return matched_value;
}

int num(Value rand){ // 得到一般形式下的分子
    if(rand->v_type == V_INT){
        return dynamic_cast<Integer*>(rand.get())->n;
    }
    if(rand->v_type == V_RATIONAL){
        return dynamic_cast<Rational*>(rand.get())->numerator;
    }
    throw RuntimeError("");
}

int den(Value rand){ //得到一般形式下的分母
    if(rand->v_type == V_INT){
        return 1;
    }
    if(rand->v_type == V_RATIONAL){
        return dynamic_cast<Rational*>(rand.get())->denominator;
    }
    throw RuntimeError("");
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // 二元加法
    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int n1 = num(rand1);
        int n2 = num(rand2);
        return IntegerV(n1+n2);
    }
    if((rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL)||(rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL)|| (rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL)){
        int num1,num2,den1,den2;
        num1 = num(rand1);
        num2 = num(rand2);
        den1 = den(rand1);
        den2 = den(rand2);
        return RationalV(num1*den2+num2*den1,den1*den2);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // 二元减法 rand1 - rand2
    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int n1 = num(rand1);
        int n2 = num(rand2);
        return IntegerV(n1-n2);
    }
    if((rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL)||(rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL)|| (rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL)){
        int num1,num2,den1,den2;
        num1 = num(rand1);
        num2 = num(rand2);
        den1 = den(rand1);
        den2 = den(rand2);
        return RationalV(num1*den2-num2*den1,den1*den2);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // 二元乘法
    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int n1 = num(rand1);
        int n2 = num(rand2);
        return IntegerV(n1*n2);
    }
    if((rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL)||(rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL)|| (rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL)){
        int num1,num2,den1,den2;
        num1 = num(rand1);
        num2 = num(rand2);
        den1 = den(rand1);
        den2 = den(rand2);
        int ans_num = num1*num2;
        int ans_den = den1*den2;
        if(ans_num%ans_den==0)return IntegerV(ans_num/ans_den);
        else return RationalV(ans_num,ans_den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // 二元除法 rand1 / rand2
    if(rand1->v_type==V_INT&&rand2->v_type==V_INT){
        int n1 = num(rand1);
        int n2 = num(rand2);
        if(n2 == 0){
            throw RuntimeError("");
        }
        if(n1%n2 == 0){
            return IntegerV(n1/n2);
        }
        else {
            return RationalV(n1,n2);
        }
    }
    if((rand1->v_type==V_INT&&rand2->v_type==V_RATIONAL)||(rand2->v_type==V_INT&&rand1->v_type==V_RATIONAL)|| (rand1->v_type==V_RATIONAL&&rand2->v_type==V_RATIONAL)){
        int num1,num2,den1,den2;
        num1 = num(rand1);
        num2 = num(rand2);
        den1 = den(rand1);
        den2 = den(rand2);
        int ans_num = num1*den2;
        int ans_den = den1*num2;
        if(ans_den == 0){
            throw RuntimeError("");
        }
        if(ans_num%ans_den == 0){
            return IntegerV(ans_num/ans_den);
        }
        else {
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

Value PlusVar::evalRator(const std::vector<Value> &args) { // 多元加法
    if(args.empty()){
        return IntegerV(0);//特判
    }
    Value ans=args[0];
    for(int i=1;i<args.size();i++){
        Plus plus(nullptr,nullptr);
        ans=plus.evalRator(ans,args[i]);
    }//包含了一元
    return ans;
}


Value MinusVar::evalRator(const std::vector<Value> &args) { // 多元减法

    if(args.empty()){
        throw(RuntimeError(""));//特判
    }
    Value ans=args[0];
    if(args.size()==1){//一元取相反数
        Minus minus(nullptr,nullptr);
        ans=minus.evalRator(IntegerV(0),ans);
        return ans;
    }
    for(int i=1;i<args.size();i++){
        Minus minus(nullptr,nullptr);
        ans=minus.evalRator(ans,args[i]);
    }
    return ans;
}

Value MultVar::evalRator(const std::vector<Value> &args) { // 多元乘法

    if(args.empty()){
        return IntegerV(1);//特判
    }
    Value ans=args[0];
    for(int i=1;i<args.size();i++){
        Mult mult(nullptr,nullptr);
        ans=mult.evalRator(ans,args[i]);
    }//包含一元
    return ans;
}

Value DivVar::evalRator(const std::vector<Value> &args) { // 多元除法
    if(args.empty()){
        throw(RuntimeError(""));//特判
    }
    if(args.size()==1){//一元取倒数
        Div div(nullptr,nullptr);
        return div.evalRator(IntegerV(1),args[0]);
    }
    Value ans=args[0];
    for(int i=1;i<args.size();i++){
        Div div(nullptr,nullptr);
        ans = div.evalRator(ans,args[i]);
    }
    return ans;
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

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //TODO: To complete the less logic
    if((rand1->v_type==V_INT||rand1->v_type==V_RATIONAL)&&(rand2->v_type==V_INT||rand2->v_type==V_RATIONAL)){
        int num1 = num(rand1);
        int num2 = num(rand2);
        int den1 = den(rand1);
        int den2 = den(rand2);
        return BooleanV(num1*den2 < num2*den1);//rational类保证den恒大于0 故可如此判定
    }
    throw RuntimeError("Wrong typename");
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //TODO: To complete the lesseq logic
    if((rand1->v_type==V_INT||rand1->v_type==V_RATIONAL)&&(rand2->v_type==V_INT||rand2->v_type==V_RATIONAL)){
        int num1 = num(rand1);
        int num2 = num(rand2);
        int den1 = den(rand1);
        int den2 = den(rand2);
        return BooleanV(num1*den2 <= num2*den1);
    }
    throw RuntimeError("Wrong typename");
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    if((rand1->v_type==V_INT||rand1->v_type==V_RATIONAL)&&(rand2->v_type==V_INT||rand2->v_type==V_RATIONAL)){
        int num1 = num(rand1);
        int num2 = num(rand2);
        int den1 = den(rand1);
        int den2 = den(rand2);
        return BooleanV(num1*den2 == num2*den1);
    }
    throw RuntimeError("Wrong typename");
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //TODO: To complete the greatereq logic
    if((rand1->v_type==V_INT||rand1->v_type==V_RATIONAL)&&(rand2->v_type==V_INT||rand2->v_type==V_RATIONAL)){
        int num1 = num(rand1);
        int num2 = num(rand2);
        int den1 = den(rand1);
        int den2 = den(rand2);
        return BooleanV(num1*den2 >= num2*den1);
    }
    throw RuntimeError("Wrong typename");
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //TODO: To complete the greater logic
    if((rand1->v_type==V_INT||rand1->v_type==V_RATIONAL)&&(rand2->v_type==V_INT||rand2->v_type==V_RATIONAL)){
        int num1 = num(rand1);
        int num2 = num(rand2);
        int den1 = den(rand1);
        int den2 = den(rand2);
        return BooleanV(num1*den2 > num2*den1);
    }
    throw RuntimeError("Wrong typename");
}

bool isfalse(Value rand){
    if(rand->v_type == V_BOOL && !dynamic_cast<Boolean*>(rand.get())->b){
        return true;//只有bool类型的false为false 即#f为false
    }
    return false;
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //TODO: To complete the less logic
    for(int i=1;i<args.size();i++){
        Less less(nullptr,nullptr);
        Value now=less.evalRator(args[i-1],args[i]);
        if(isfalse(now)){
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //TODO: To complete the lesseq logic
    for(int i=1;i<args.size();i++){
        LessEq lesseq(nullptr,nullptr);
        Value now=lesseq.evalRator(args[i-1],args[i]);
        if(isfalse(now)){
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //TODO: To complete the equal logic
    for(int i=1;i<args.size();i++){
        Equal equal(nullptr,nullptr);
        Value now=equal.evalRator(args[i-1],args[i]);
        if(isfalse(now)){
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //TODO: To complete the greatereq logic
    for(int i=1;i<args.size();i++){
        GreaterEq greatereq(nullptr,nullptr);
        Value now=greatereq.evalRator(args[i-1],args[i]);
        if(isfalse(now)){
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //TODO: To complete the greater logic
    for(int i=1;i<args.size();i++){
        Greater greater(nullptr,nullptr);
        Value now=greater.evalRator(args[i-1],args[i]);
        if(isfalse(now)){
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //TODO: To complete the cons logic
    return PairV(rand1,rand2);//构造一个对
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //TODO: To complete the list logic
    Value list=NullV();//逆序构造一个列表 (a,(b,(c,)))
    for(int i=args.size()-1;i>=0;i--){
        list=PairV(args[i],list);
    }
    return list;
}

Value IsList::evalRator(const Value &rand) { // list?
    //TODO: To complete the list? logic
    Value now=rand;
    while(now->v_type==V_PAIR){
        now=dynamic_cast<Pair*>(now.get())->cdr;
    }
    if(now->v_type == V_NULL){
        return BooleanV(true);
    }
    else {
        return BooleanV(false);
    }//判断是否为(a,(b,(c,)))的形式
}

Value Car::evalRator(const Value &rand) { // car
    //TODO: To complete the car logic
    if(rand->v_type == V_PAIR) {
        Pair* p = dynamic_cast<Pair*>(rand.get());
        return p->car;
    }
    throw(RuntimeError("Wrong typename in Car"));
}

Value Cdr::evalRator(const Value &rand) { // cdr
    //TODO: To complete the cdr logic
    if(rand->v_type == V_PAIR) {
        Pair* p = dynamic_cast<Pair*>(rand.get());
        return p->cdr;
    }
    throw(RuntimeError("Wrong typename in Cdr"));
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //TODO: To complete the set-car! logic 修改
    if(rand1->v_type!=V_PAIR){
        throw(RuntimeError("Wrong typename"));
    }
    Pair *p=dynamic_cast<Pair*>(rand1.get());
    p->car=rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //TODO: To complete the set-cdr! logic 修改
    if(rand1->v_type!=V_PAIR){
        throw(RuntimeError("Wrong typename"));
    }
    Pair *p=dynamic_cast<Pair*>(rand1.get());
    p->cdr=rand2;
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
    Value ans=VoidV();
    for(auto &it:es)
        ans=it->eval(e);
    return ans;
}

Value Quote::eval(Assoc& e) {
    //TODO: To complete the quote logic
    //分为一般类型和列表类型
    //列表(T_T)
    if (auto num = dynamic_cast<Number*>(s.get())) {
        return IntegerV(num->n);
    } else if (auto rational = dynamic_cast<RationalSyntax*>(s.get())) {
        return RationalV(rational->numerator, rational->denominator);
    } else if (auto str = dynamic_cast<StringSyntax*>(s.get())) {
        return StringV(str->s);
    } else if (auto sym = dynamic_cast<SymbolSyntax*>(s.get())) {
        return SymbolV(sym->s);
    } else if (dynamic_cast<TrueSyntax*>(s.get())) {
        return BooleanV(true);
    } else if (dynamic_cast<FalseSyntax*>(s.get())) {
        return BooleanV(false);
    }
    else if (auto list_syn = dynamic_cast<List*>(s.get())) {
        if (list_syn->stxs.empty()) {
            return NullV();
        }
        int dot_pos = -1;
        for (int i = 0; i < list_syn->stxs.size(); ++i) {
            if (SymbolSyntax* sym = dynamic_cast<SymbolSyntax*>(list_syn->stxs[i].get())) {
                if (sym->s == ".") {
                    dot_pos = i;
                }
            }
        }
        if (dot_pos != -1) {// have dot
            Value car = NullV();
            for (int i = dot_pos - 1; i >= 0; --i) {
                car = PairV(Quote(list_syn->stxs[i]).eval(e), car);//构造前半部分
            }
            Value cdr = Quote(list_syn->stxs[dot_pos + 1]).eval(e);//后半
            //dot_pos 是 "." 的位置，过
            if (car->v_type == V_NULL) {
                return cdr;
            } else {
                Value now = car;
                while (true) {
                    Pair* pair = dynamic_cast<Pair*>(now.get());
                    if (pair->cdr->v_type == V_NULL) {
                        pair->cdr = cdr;
                        break;
                    }
                    now = pair->cdr;
                }
                return car;
            }
        }
        Value ans = NullV();
        for (int i = list_syn->stxs.size() - 1; i >= 0; --i) {//没有dot 纯链表，对每一个元素求quote再逆序构造链表
            ans = PairV(Quote(list_syn->stxs[i]).eval(e), ans);
        }
        return ans;
    }
    throw RuntimeError("");
}
Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //TODO: To complete the and logic
    if(rands.empty()) {
        return BooleanV(true);
    }
    else {
        Value last=BooleanV(true);
        for(auto &it:rands){
            Value val=it->eval(e);
            if(isfalse(val)){
                return BooleanV(false);
            }
            last=val;
        }
        return last;
    }
    
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //TODO: To complete the or logic
    
    if(rands.empty()) return BooleanV(false);
    else {
        Value last=BooleanV(false);
        for(auto &it : rands) {
            Value val=it->eval(e);
            if(isfalse(val)){
                continue; 
            }
            return val; 
        }
        return last;
    }
    
}

Value If::eval(Assoc &e) {
    Value val = cond->eval(e);
    if (isfalse(val)){
        return alter->eval(e);
    }
    else{
        return conseq->eval(e);
    }
}

Value Not::evalRator(const Value &rand) {
    if(isfalse(rand)){
        return BooleanV(true);
    }
    else {
        return BooleanV(false);
    }
}

Value Cond::eval(Assoc &env) {
    //TODO: To complete the cond logic
    for(auto &clause:clauses){
        if(clause.empty()){
            continue;
        }
        bool flag=false;
        if(Var* varx=dynamic_cast<Var*>(clause[0].get())){
            if(varx->x=="else"){
                flag=true;
            }
        }
        Value val = VoidV();
        if(flag == true){
            val = BooleanV(true);
        }
        else {
            val = clause[0]->eval(env);
        }
        if(!isfalse(val)){
            Value ans=VoidV();
            for(int i=1;i<clause.size();i++) {
                ans=clause[i]->eval(env);
            }
            return ans;
        } 
    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) { 
    //TODO: To complete the lambda logic
    return ProcedureV(x,e,env);
}

Value Apply::eval(Assoc &env) {
    Value proc_val = rator->eval(env);
    if (proc_val->v_type != V_PROC) {
        throw RuntimeError("Attempt to apply a non-procedure");
    }

    Procedure* proc = dynamic_cast<Procedure*>(proc_val.get());
    std::vector<Value> arg_vals;
    
    for(auto &arg_expr : rand) {
        arg_vals.push_back(arg_expr->eval(env));
    }
    if (auto varNode = dynamic_cast<Variadic*>(proc->e.get())) {
        //TODO
        return varNode->evalRator(arg_vals);
    }
    if (arg_vals.size() != proc->parameters.size()) {
        throw RuntimeError("Wrong number of arguments");
    }
    
    Assoc new_env = proc->env;
    for(size_t i = 0; i < arg_vals.size(); i++) {
        new_env = extend(proc->parameters[i], arg_vals[i], new_env);
    }
    
    return proc->e->eval(new_env);
}

Value Define::eval(Assoc &env){
    Assoc newenv = extend(var, Value(nullptr), env);
    Value newValue = e->eval(newenv);
    modify(var, newValue, newenv);
    env = newenv;
    return VoidV();
}

Value Let::eval(Assoc &env) {
    //TODO: To complete the let logic
    Assoc newenv=env;
    for(auto &it:bind){
        Value val=it.second->eval(env);
        newenv=extend(it.first,val,newenv);
    }
    return body->eval(newenv);
}

Value Letrec::eval(Assoc &env) {
    //TODO: To complete the letrec logic
    Assoc newenv=env;
    for(auto &it:bind)
        newenv=extend(it.first,VoidV(),newenv);
    for(auto &it:bind){
        Value val=it.second->eval(newenv);
        modify(it.first,val,newenv);
    }
    return body->eval(newenv);
}

Value Set::eval(Assoc &env) {
    //TODO: To complete the set logic
    Value val=e->eval(env);
    Value flag=find(var,env);
    if(flag.get()==nullptr){
        throw(RuntimeError("Undefined variable : " + var));
    }
    modify(var,val,env);
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