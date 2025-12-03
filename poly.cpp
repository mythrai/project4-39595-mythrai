#include "poly.h"

#include <iostream>
#include <algorithm>
#include <stdexcept>

void polynomial::normal_coeff()
{
    while (!coeffTerms.empty() && coeffTerms.back() == 0)
    {
        coeffTerms.pop_back();
    }

    if (coeffTerms.empty())
    {
        coeffTerms.push_back(0);
    }
}

size_t polynomial::degree() const
{
    if (coeffTerms.empty())
    {
        return 0;
    }

    for (size_t myth = coeffTerms.size(); myth-- > 0; )
    {
        if (coeffTerms[myth] != 0)
        {
            return myth;
        }
    }
    return 0;
}

/**
 * @brief Construct a new polynomial object that is the number 0 (ie. 0x^0)
 */
polynomial::polynomial()
{
    coeffTerms.assign(1, 0); // 0 * x^0
}

/**
 * @brief Construct a new polynomial object from an existing polynomial object
 */
polynomial::polynomial(const polynomial &other)
    : coeffTerms(other.coeffTerms)
{
}

/**
 * @brief Prints the polynomial.
 *
 * Only used for debugging, isn't graded.
 */
void polynomial::print() const
{
    auto can = canonical_form();

    if (can.empty())
    {
        std::cout << "0";
        return;
    }

    bool firstTerm = true;
    for (const auto &term : can)
    {
        power ra = term.first;
        coeff c = term.second;

        if (!firstTerm)
        {
            std::cout << (c >= 0 ? " + " : " - ");
        }
        else if (c < 0)
        {
            std::cout << "-";
        }

        int absCoeff = (c >= 0) ? c : -c;

        if (ra == 0 || absCoeff != 1)
        {
            std::cout << absCoeff;
        }
        if (ra > 0)
        {
            std::cout << "x";
            if (ra > 1)
            {
                std::cout << "^" << ra;
            }
        }

        firstTerm = false;
    }
}

/**
 * @brief Assignment operator – deep copy.
 */
polynomial &polynomial::operator=(const polynomial &other)
{
    if (this != &other)
    {
        coeffTerms = other.coeffTerms;
    }
    return *this;
}

/**
 * @brief polynomial + polynomial
 */
polynomial polynomial::operator+(const polynomial &rhs) const
{
    polynomial SumRes;

    size_t sizeMax = std::max(coeffTerms.size(), rhs.coeffTerms.size());
    SumRes.coeffTerms.assign(sizeMax, 0);

    for (size_t rk = 0; rk < sizeMax; ++rk)
    {
        coeff left  = (rk < coeffTerms.size())      ? coeffTerms[rk]      : 0;
        coeff right = (rk < rhs.coeffTerms.size())  ? rhs.coeffTerms[rk]  : 0;

        SumRes.coeffTerms[rk] = left + right;
    }

    SumRes.normal_coeff();
 
    return SumRes;
}

/**
 * @brief polynomial * polynomial
 */
polynomial polynomial::operator*(const polynomial &rhs) const
{
    polynomial MultRes;

    if ((coeffTerms.size() == 1 && coeffTerms[0] == 0) ||
        (rhs.coeffTerms.size() == 1 && rhs.coeffTerms[0] == 0))
    {
        MultRes.coeffTerms.assign(1, 0);
    
        return MultRes;   // FIX: was returning SumRes before
    }

    size_t leftDeg  = degree();
    
    size_t rightDeg = rhs.degree();
    
    size_t resultDeg = leftDeg + rightDeg;

    MultRes.coeffTerms.assign(resultDeg + 1, 0);

    for (size_t mrk = 0; mrk <= leftDeg; ++mrk)
    {
        if (coeffTerms[mrk] == 0) continue;
       
        for (size_t j = 0; j <= rightDeg; ++j)
        {
            if (rhs.coeffTerms[j] == 0) continue;
            MultRes.coeffTerms[mrk + j] += coeffTerms[mrk] * rhs.coeffTerms[j];
        }
    }

    MultRes.normal_coeff();
    return MultRes;
}

/**
 * @brief polynomial % polynomial (remainder of long division)
 */
polynomial polynomial::operator%(const polynomial &divisor) const
{
    if (divisor.coeffTerms.size() == 1 && divisor.coeffTerms[0] == 0)
    {
        throw std::invalid_argument("Modulo by zero polynomial");
    }

    polynomial remainD(*this);

    size_t divisorDeg = divisor.degree();
   
    coeff leadDiv = divisor.coeffTerms[divisorDeg];

    while (!(remainD.coeffTerms.size() == 1 && remainD.coeffTerms[0] == 0))
    {
        size_t deg_rem = remainD.degree();
        if (deg_rem < divisorDeg)
        {
            break;
        }

        coeff lead_rem = remainD.coeffTerms[deg_rem];
        coeff factor = lead_rem / leadDiv;

        power diffPower = deg_rem - divisorDeg;

        for (size_t wr = 0; wr <= divisorDeg; ++wr)
        {
            coeff coeffDiv = (wr < divisor.coeffTerms.size()) ? divisor.coeffTerms[wr] : 0;
            if (coeffDiv != 0)
            {
                remainD.coeffTerms[wr + diffPower] -= factor * coeffDiv;
            }
        }

        remainD.normal_coeff();
    }

    return remainD;
}

/**
 * @brief Returns the degree of the polynomial (non-const wrapper)
 */
size_t polynomial::find_degree_of()
{
    return degree();
}

/**
 * @brief Returns the canonical form of the polynomial.
 */
std::vector<std::pair<power, coeff>> polynomial::canonical_form() const
{
    std::vector<std::pair<power, coeff>> result;

    size_t degRe = degree();

    if (degRe == 0 && (coeffTerms.empty() || coeffTerms[0] == 0))
    {
        result.emplace_back(0, 0);
   
        return result;
    }

    for (size_t pr = coeffTerms.size(); pr-- > 0; )
    {
        coeff c = coeffTerms[pr];
        if (c != 0)
        {
            result.emplace_back(pr, c);
        }
    }

    if (result.empty())
    {
        result.emplace_back(0, 0);
    }

    return result;
}

/**
 * @brief polynomial + int
 */
polynomial operator+(const polynomial &lhs, int rhs)
{
    polynomial tmp_const;
    tmp_const.coeffTerms.assign(1, rhs);
 
    tmp_const.normal_coeff();

    return lhs + tmp_const;
}

/**
 * @brief int + polynomial
 */
polynomial operator+(int lhs, const polynomial &rhs)
{
    return rhs + lhs;
}

/**
 * @brief polynomial * int
 */
polynomial operator*(const polynomial &lhs, int rhs)
{
    polynomial result(lhs);

    for (size_t rm = 0; rm < result.coeffTerms.size(); ++rm)
    {
        result.coeffTerms[rm] *= rhs;
    }
    result.normal_coeff();

    return result;
}

/**
 * @brief int * polynomial
 */
polynomial operator*(int lhs, const polynomial &rhs)
{
    return rhs * lhs;
}

