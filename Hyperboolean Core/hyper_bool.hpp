/**
 * @file hyper_bool.hpp
 * @brief Implementation of a hyper-boolean class with multiple-valued logic
 * @details This class implements a hyper-boolean system that can handle multiple-valued logic
 *          with configurable truth values and different logical operations based on various
 *          mathematical systems (Kleeene, Lukasiewicz, Godel, Post, Zhu_Xiao).
 */

#ifndef HYPER_BOOL_HPP_
#define HYPER_BOOL_HPP_

#include<cmath>
#include<functional>
#include<string>

/**
 * @class hbool
 * @brief A class implementing hyper-boolean logic with multiple values
 * 
 * The hbool class supports multiple-valued logic operations with configurable
 * number of possible values and different mathematical systems for logical operations.
 */
class hbool{
		unsigned char value;  ///< The current truth value of the hbool object
		static unsigned char max_value;  ///< Maxium of the truth values in the system
		/**
		 * @struct settings
		 * @brief Configuration flags for the hbool system
		 */
		static struct{
			unsigned not_normal:1;      ///< Flag for non-standard negation behavior
			unsigned not_symmetry:1;    ///< Flag for symmetric negation behavior
			unsigned imply_strong:1;    ///< Flag for strong implication behavior
			unsigned or_strong:1;       ///< Flag for strong OR behavior
		}settings;

	public:
        /**
         * @brief Constructor from various numeric types
         * @details Converts input to valid truth value by clamping to range [0, max_value]
         */
        hbool(const char a=0){value=a<0?0:(a>max_value?max_value:a);}
        hbool(const unsigned char a){value=a>max_value?max_value:a;}
        hbool(const short a){value=a<0?0:(a>max_value?max_value:a);}
        hbool(const unsigned short a){value=a>max_value?max_value:a;}
        hbool(const int a){value=a<0?0:(a>max_value?max_value:a);}
		hbool(const unsigned a){value=a>max_value?max_value:a;}
        hbool(const long a){value=a<0?0:(a>max_value?max_value:a);}
		hbool(const unsigned long a){value=a>max_value?max_value:a;}
		hbool(const long long a){value=a<0?0:(a>max_value?max_value:a);}
		hbool(const unsigned long long a){value=a>max_value?max_value:a;}
        /**
         * @brief Constructor from bool
         * @details Maps false to 0 and true to max_value
         */
        hbool(const bool a){value=a?max_value:0;}
        /**
         * @brief Constructor from floating point types
         * @details Maps [0,1] to [0, max_value] by rounding
         */
		hbool(const float a){value=a<0?0:(a>1?max_value:round(a*max_value));}
		hbool(const double a){value=a<0?0:(a>1?max_value:round(a*max_value));}

        /**
         * @brief Conversion operator to various numeric types
         * @return The underlying truth value
         */
		explicit operator char()const{return value;}
		explicit operator unsigned char()const{return value;}
		explicit operator short()const{return value;}
		explicit operator unsigned short()const{return value;}
		explicit operator int()const{return value;}
		explicit operator unsigned()const{return value;}
		explicit operator long()const{return value;}
		explicit operator unsigned long()const{return value;}
		explicit operator long long()const{return value;}
		explicit operator unsigned long long()const{return value;}

		/**
		 * @brief Conversion operator to bool
		 * @return true if value > 0.5, false otherwise
		 */
		explicit operator bool()const{return value>max_value/2;}

		/**
		 * @brief Conversion operator to floating point types
		 * @return The current truth value as a floating point type
		 */
		explicit operator float()const{return 1.*value/max_value;}
		explicit operator double()const{return 1.*value/max_value;}

		/**
		 * @brief Static method to configure the hbool system
		 * @param Possible_value Number of possible truth values
		 * @param Not_normal Flag for non-standard negation
		 * @param Not_symmetry Flag for symmetric negation
		 * @param Imply_strong Flag for strong implication
		 * @param Or_strong Flag for strong OR operation
		 */
		static void set(char Max_value=1,bool Not_normal=1,bool Not_symmetry=1,bool Imply_strong=1,bool Or_strong=1){
			max_value=Max_value;
			settings.not_normal=Not_normal;
			settings.not_symmetry=Not_symmetry;
			settings.imply_strong=Imply_strong;
			settings.or_strong=Or_strong;
		}

		/**
		 * @brief Negation operator
		 * @return The negated hbool value
		 * @details The negation behavior depends on the current settings:
		 *          - not_normal=1, not_symmetry=1: Kleeene-Lukasiewicz negation (1-value)
		 *          - not_normal=1, not_symmetry=0: Godel negation (value?0:1)
		 *          - not_normal=0, not_symmetry=1: Post negation
		 *          - not_normal=0, not_symmetry=0: Zhu-Xiao negation
		 */
		inline hbool operator!()const{
			return
				settings.not_normal?(
					settings.not_symmetry?
					(max_value-value)//Kleeene_Lukasiewicz
					:
					(value?0:max_value)//Godel
				)
				:(
					settings.not_symmetry?
					(value?value-1:max_value)//Post
					:
					(value+(value==max_value?-1:1))//Zhu_Xiao
				);
		}

		/**
		 * @brief Implication operation
		 * @param a The right operand
		 * @return The result of the implication operation
		 * @details The implication behavior depends on the imply_strong setting:
		 *          - imply_strong=1: Lukasiewicz implication
		 *          - imply_strong=0: Kleeene implication
		 */
		inline hbool imply(const hbool a)const{
			return
			settings.imply_strong?
			(value<a.value?max_value:max_value-value+a.value)//Lukasiewicz
			:
			(value+a.value>max_value?a.value:max_value-value)//Kleeene
			;
		}

		/**
		 * @brief Logical OR operation
		 * @param a The right operand
		 * @return The result of the OR operation
		 * @details The OR behavior depends on the or_strong setting:
		 *          - or_strong=1: Lukasiewicz OR
		 *          - or_strong=0: Kleeene OR
		 */
		inline hbool operator||(const hbool a)const{
			return
			(settings.or_strong?
			imply(a)//Lukasiewicz
			:
			!(*this)//Kleeene
			).imply(a);
		}

		/**
		 * @brief Logical AND operation
		 * @param a The right operand
		 * @return The result of the AND operation
		 * @details Implemented using De Morgan's laws: a && b = !(!a || !b)
		 */
		inline hbool operator&&(const hbool a)const{return !(!(*this)||!a);}

		/**
		 * @brief Equality comparison
		 * @param a The right operand
		 * @return The result of the equality comparison
		 * @details Implemented using bidirectional implication: a == b = (a->b) && (b->a)
		 */
		inline hbool operator==(const hbool a)const{return imply(a)&&a.imply(*this);}
		
		/**
		 * @brief Get the number of possible truth values
		 * @return Number of possible truth values in the system
		 */
		static unsigned char get_max_value(){return max_value;}
};

// Initialize static members
unsigned char hbool::max_value=1;  ///< Default number of maxium values is 1 (true/false)
decltype(hbool::settings)hbool::settings={1,1,1,1};  ///< Default settings enable all special behaviors

/**
 * @brief Universal quantifier evaluation for all possible boolean values
 * @param func Function pointer taking hbool and returning hbool, defaults to lambda returning 1
 * @return hbool Returns logical AND of func results across all possible values
 * @details Evaluates func(x) for all possible x in the system and returns the minimum result
 */
hbool for_all(std::function<hbool(hbool)> func=[](hbool a)->hbool{return 1.;}) {
    hbool ans(1.),current_result;  // Initialize result as true (1)
    // Iterate through all possible hbool values (0 to 1, step = 1/(possible_values-1))
	unsigned char current_value = 0;
	do{
		current_result = func(hbool(current_value));  // Evaluate func for current value
		// Update ans to be the minimum of current ans and func(current_value)
		ans = char(ans) > char(current_result) ? current_result : ans;
		current_value++;
	}while(current_value!=char(hbool::get_max_value()+1));
    return ans;  // Return final result
}

/**
 * @brief Existential quantifier evaluation for possible boolean values
 * @param func Function pointer taking hbool and returning hbool, defaults to lambda returning 1
 * @return hbool Returns whether there exists a value making func return true
 * @details Implemented using De Morgan's laws.
 */
hbool exist(std::function<hbool(hbool)> func=[](hbool a)->hbool{return 1.;}) {
    // Existential quantifier is equivalent to NOT universal quantifier of negation
    return !for_all([func](hbool a)->hbool{return !func(a);});
}

/**
 * @brief Template class for fuzzy set operations using hbool values
 * @tparam _Tp Type of elements in the set
 */
template<typename _Tp=int>
struct hset{
		// Function object to determine the degree to which an element belongs to the set
		// Type: std::function that takes a _Tp parameter and returns hbool
		std::function<hbool(_Tp)> find;
        
		/**
		 * @brief Constructor
		 * @param find Function to determine membership degree
		 */
		hset(std::function<hbool(_Tp)> find=[](const _Tp& a)->hbool{return 0;}):find(find){}
		
		/**
		 * @brief Assignment operator
		 * @param a Another hset object
		 * @return Reference to current object
		 */
		hset& operator=(const hset& a){
			find=a.find;
			return *this;
		}
		hset& operator=(const std::function<hbool(_Tp)> a){
			find=a;
			return *this;
		}
		
		/**
		 * @brief Union operation
		 * @param a Another hset object
		 * @return New set containing elements from either set
		 */
		hset U(const hset& a)const{
			return hset([this,a](const _Tp& b)->hbool{return find(b)||a.find(b);});
		}
		
		/**
		 * @brief Intersection operation
		 * @param a Another hset object
		 * @return New set containing elements common to both sets
		 */
		hset N(const hset& a)const{
			return hset([this,a](const _Tp& b)->hbool{return find(b)&&a.find(b);});
		}
		
		/**
		 * @brief Set difference operation
		 * @param a Another hset object
		 * @return New set containing elements in this set but not in parameter set
		 */
		hset operator-(const hset& a)const{
			return hset([this,a](const _Tp& b)->hbool{return find(b)&&!a.find(b);});
		}
		
		/**
		 * @brief Calculates the symmetric difference between two sets.
		 *
		 * The symmetric difference of two sets A and B is the set of elements which are in either of the sets,
		 * but not in their intersection. This can be computed as (A - B) U (B - A), where
		 * U is the union operation and - is the set difference operation.
		 *
		 * @param a A constant reference to an hset object, which is the other operand for the symmetric difference.
		 * @return hset A new hset object containing the symmetric difference of the current set and set 'a'.
		 */
		hset SD(const hset& a)const
		{
		    // Compute and return the symmetric difference by:
		    // 1. Calculating the difference between the current set and 'a' (*this - a).
		    // 2. Calculating the difference between 'a' and the current set (a - *this).
		    // 3. Taking the union of these two resulting sets.
		    return ((*this)-a).U(a-(*this));
		}

		/**
		 * @brief Complement operation
		 * @return New set containing elements not in this set
		 */
		hset C()const{return hset([this](const _Tp& b)->hbool{return !find(b);});}

		/**
		 * @brief Insert an element into the set
		 * @param a The element to be inserted
		 * @param extent Hyper_boolean value indicating the degree of membership of element in the set
		 * @note This method updates the find function object to implement element insertion
		 *       Uses a lambda expression that captures current find state and updates membership for element a
		 */
		void insert(_Tp a, hbool extent) {
			find = [this, a, extent](const _Tp& b) -> hbool {
				return b == a ? extent : find(b);
			};
		}
		
		/**
		 * @brief Remove an element from the set
		 * @param a The element to be removed
		 * @param extent Hyper_boolean value indicating the degree of non-membership of element in the set
		 * @note This method updates the find function object to implement element removal
		 *       Uses a lambda expression that captures current find state and updates membership for element a
		 *       Uses !extent to invert the extent value
		 */
		void erase(_Tp a, hbool extent) {insert(a, !extent);}
};

/**
 * @brief Global array storing proposition values
 * @details Used by formula_node for variable evaluation
 */
hbool p[0x10000];

/**
 * @enum Type
 * @brief Enumeration of formula node types
 */
enum Type{
    VAR,    ///< Variable node
    CONST,  ///< Constant value node
    NOT,    ///< Negation operation
    AND,    ///< Logical AND operation
    OR,     ///< Logical OR operation
    IMPLY,  ///< Implication operation
    EQ      ///< Equality operation
};

/**
 * @struct formula_node
 * @brief Node structure for building and evaluating logical formulas
 * 
 * This structure represents a node in a logical formula tree. Each node can be:
 * - A variable reference
 * - A constant value
 * - A logical operation with one or two operands
 * 
 * The structure supports:
 * - Tree traversal and evaluation
 * - Formula manipulation
 * - String representation
 * - Memory management
 */
struct formula_node{
    formula_node* left;
    formula_node* right;
    int var_idx;
    hbool const_val;
    Type type;
    formula_node(Type type, hbool value , int index , formula_node* left = nullptr, formula_node* right = nullptr) : type(type), const_val(value), var_idx(index), left(left), right(right) {}
    formula_node(const formula_node& other)
        : type(other.type), const_val(other.const_val), var_idx(other.var_idx),
          left(other.left ? other.left->clone() : nullptr),
          right(other.right ? other.right->clone() : nullptr) {}
    formula_node* clone() const {return new formula_node(*this);}
    ~formula_node() {
        delete left;
        delete right;
    }
    int depth() const {
        if(type == VAR || type == CONST) return 0;
        if(type == NOT) return 1 + (left?left->depth():0);
        return 1 + std::max(left?left->depth():0, right?right->depth():0);
    }
    int size() const {
        if(type == VAR || type == CONST) return 1;
        if(type == NOT) return 1 + (left?left->size():0);
        return 1 + (left?left->size():0) + (right?right->size():0);
    }
    int max_var_idx() const {
        if(type == CONST) return -1;
        if(type == VAR) return var_idx;
        if(type == NOT) return left?left->max_var_idx():-1;
        return std::max(left?left->max_var_idx():-1, right?right->max_var_idx():-1);
    }
    hbool eval() const {
        switch(type) {
            case VAR:
                return p[var_idx];
            case CONST:
                return const_val;
            case NOT:
                return !left->eval();
            case AND:
                return left->eval() && right->eval();
            case OR:
                return left->eval() || right->eval();
            case IMPLY:
                return left->eval().imply(right->eval());
            case EQ:
                return left->eval() == right->eval();
        }
    }
    std::string to_string() const {
        switch(type) {
            case VAR:
                return "p" + std::to_string(var_idx);
            case CONST:
                return std::to_string((int)char(const_val));
            case NOT:
                return "( !" + left->to_string()+" )";
            case AND:
                return "( " + left->to_string() + " && " + right->to_string() + " )";
            case OR:
                return "( " + left->to_string() + " || " + right->to_string() + " )";
            case IMPLY:
                return "( " + left->to_string() + " -> " + right->to_string() + " )";
            case EQ:
                return "( " + left->to_string() + " == " + right->to_string() + " )";
            default:
                return "?";
        }
    }
};

inline formula_node* var(int idx) {
    return new formula_node(VAR, hbool(), idx);
}

inline formula_node* constant(double value) {
    return new formula_node(CONST, hbool(value), -1);
}

inline formula_node* not_expr(formula_node* expr) {
    return new formula_node(NOT, hbool(), -1, expr);
}

inline formula_node* and_expr(formula_node* left, formula_node* right) {
    return new formula_node(AND, hbool(), -1, left, right);
}

inline formula_node* or_expr(formula_node* left, formula_node* right) {
    return new formula_node(OR, hbool(), -1, left, right);
}

inline formula_node* imply_expr(formula_node* left, formula_node* right) {
    return new formula_node(IMPLY, hbool(), -1, left, right);
}

inline formula_node* eq_expr(formula_node* left, formula_node* right) {
    return new formula_node(EQ, hbool(), -1, left, right);
}

#endif
