#include "return_codes.h"

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef enum special {
	normal, zero, infinity, nan
} special;

typedef struct floatNum
{
	int8_t sign;
	uint64_t mantissa;
	int32_t exponent;
	special special;
	// zero = 1
	// inf = 2
	// NaN = 3
} floatNum;

floatNum subtract(floatNum num1, floatNum num2, int8_t bit, int rounding);

void normalizeMantissa(floatNum *res, int8_t bit, int8_t *remains_bits, int8_t *last_bit)
{
	uint64_t temp_mantissa = (res->mantissa) >> (bit);
	if (temp_mantissa > 1)
	{
		while (temp_mantissa > 1)
		{
			*last_bit = res->mantissa & 1;
			temp_mantissa >>= 1;
			res->mantissa >>= 1;
			res->exponent++;
			if (res->mantissa & 1)
				(*remains_bits)++;
		}
	}
	else
	{
		//		if (!((1 << (bit)) & res->mantissa))
		//		{
		while (!((1 << (bit)) & res->mantissa) && res->mantissa)
		{
			res->mantissa <<= 1;
			res->exponent--;
		}
		//		}
	}
}

special specialNum(floatNum *num, int8_t bit)
{
	int32_t maskExp = (1 << ((bit == 23 ? 8 : 5) - 1));
	if ((num->exponent == (bit == 23 ? -127 : -15)) && ((num->mantissa) == (1 << (bit))))
	{
		// zero
		num->mantissa = 0;
		num->exponent = 0;
		num->special = zero;
		return zero;
	}
	else if (num->mantissa == (1 << (bit)))
	{
		// infinity
		num->exponent = maskExp;
		num->mantissa = 1 << bit;
		num->special = infinity;
		return infinity;
	}
	else if ((num->exponent == maskExp) && (num->mantissa > (1 << (bit))))
	{
		// nan
		num->exponent = maskExp;
		num->mantissa = (1 << bit) + 1;
		num->special = nan;
		return nan;
	}
	return normal;
}

void roundTo(floatNum *num, int rounding, int8_t remains_bits, int8_t last_bit)
{
	if (rounding == 1)
	{
		if (last_bit && ((remains_bits - 1) > 0))
		{
			num->mantissa++;
		}
		else if (last_bit && (num->mantissa + 1) % 2 == 0)
		{
			num->mantissa++;
		}
	}
	else if (rounding == 2 && remains_bits && num->sign == 0)
	{
		num->mantissa++;
	}
	else if (rounding == 3 && remains_bits && num->sign == 1)
	{
		num->mantissa++;
	}
}

floatNum splitNum(int64_t number, int8_t bit)
{
	floatNum fl;
	fl.sign = (number >> (bit == 23 ? 31 : 15) ? 1 : 0);
	fl.exponent = number & (bit == 23 ? 0x7f800000 : 0x7C00);
	fl.exponent >>= bit;
	fl.exponent &= (bit == 23 ? 0xFF : 0x1F);
	fl.exponent -= (bit == 23 ? 127 : 15);
	fl.mantissa = number & (bit == 23 ? 0x007fffff : 0x3FF);
	if (fl.exponent == (bit == 23 ? -127 : -15))
	{
		if (fl.mantissa > 0)
		{
			while (!(fl.mantissa & (1 << 23)))
			{
				fl.mantissa <<= 1;
				fl.exponent--;
			}
			fl.exponent++;
		}
	}
	fl.mantissa += 1 << bit;
	return fl;
}

void make_equal(floatNum *num1, floatNum *num2, int8_t *remains_bit)
{
	floatNum *max = (num1->exponent > num2->exponent ? num1 : num2);
	floatNum *min = (num1->exponent < num2->exponent ? num1 : num2);
	if ((max->exponent - min->exponent) < 39)
	{
		while (max->exponent > min->exponent)
		{
			max->mantissa <<= 1;
			(max->exponent)--;
		}
	}
	else
	{
		while (max->exponent > min->exponent)
		{
			if (min->mantissa & 1)
				(*remains_bit)++;
			min->mantissa >>= 1;
			(min->exponent)++;
		}
	}
}

special checkForSpecialDivide(floatNum *temp, floatNum *num1, floatNum *num2)
{
	if (num1->special == 3 || num2->special == 3 || (num1->special == 1 && num2->special == 1) ||
		(num1->special == 2 && num2->special == 2))
	{
		// NaN
		temp->special = nan;
		temp->sign = 0;
		return nan;
	}
	else if (num2->special == 1 || num1->special == 2)
	{
		// Inf
		temp->special = infinity;
		temp->sign = num1->sign ^ num2->sign;
		return infinity;
	}
	else if (num1->special == 1 || num2->special == 2)
	{
		// zero
		temp->mantissa = 0;
		temp->sign = num1->sign ^ num2->sign;
		temp->special = zero;
		return zero;
	}
	return normal;
}

floatNum mult(floatNum num1, floatNum num2, int8_t bit, int rounding)
{
	special specialNum1 = specialNum(&num1, bit);
	special specialNum2 = specialNum(&num2, bit);
	if ((specialNum1 == zero && specialNum2 == infinity) || (specialNum1 == infinity && specialNum2 == zero) || specialNum1 == nan || specialNum2 == nan)
	{
		// Nan
		floatNum r = { 0, 0, 0, nan };
		return r;
	}
	else if (specialNum1 == infinity || specialNum2 == infinity)
	{
		// Zero with sign
		floatNum r = { num1.sign ^ num2.sign, 0, 0, infinity };
		return r;
	}
	floatNum res = { 0, (uint64_t)((uint64_t)num1.mantissa * (uint64_t)num2.mantissa), num1.exponent + num2.exponent - bit };
	int8_t remains_bits = 0;
	int8_t last_bit = 0;
	normalizeMantissa(&res, bit, &remains_bits, &last_bit);
	specialNum(&res, bit);
	//	if (!(isSpecial))
	res.sign = num1.sign ^ num2.sign;
	roundTo(&res, rounding, remains_bits, last_bit);
	return res;
}

floatNum divide(floatNum num1, floatNum num2, int8_t bit, int rounding)
{
	floatNum temp;
	specialNum(&num1, bit);
	specialNum(&num2, bit);
	special check = checkForSpecialDivide(&temp, &num1, &num2);
	if (check != normal)
	{
		return temp;
	}
	num1.mantissa <<= 40;
	num1.exponent -= 40;
	int8_t remains_bits = 0;
	int8_t last_bit = 0;
	floatNum res = { num1.sign ^ num2.sign, (uint64_t)((uint64_t)(num1.mantissa) / (uint64_t)(num2.mantissa)), num1.exponent - num2.exponent + bit };
	normalizeMantissa(&res, bit, &remains_bits, &last_bit);
	roundTo(&res, rounding, remains_bits, last_bit);
	specialNum(&res, bit);
	return res;
}

special checkForSpecialAddSubtract(floatNum *temp, floatNum *num1, floatNum *num2)
{
	if ((num1->special == infinity && num2->special == infinity) && (num1->sign != num2->sign))
	{
		temp->sign = 0;
		temp->mantissa = 0;
		temp->exponent = 0;
		temp->special = nan;
		return nan;
	}
	return normal;
}
floatNum add(floatNum num1, floatNum num2, int8_t bit, int rounding)
{
	specialNum(&num1, bit);
	specialNum(&num2, bit);
	floatNum temp;
	special check = checkForSpecialAddSubtract(&temp, &num1, &num2);
	if (check)
	{
		return temp;
	}
	if (num1.sign == 1 && num2.sign == 0)
	{
		num1.sign = 0;
		return subtract(num2, num1, bit, rounding);
	}
	else if (num1.sign == 0 && num2.sign == 1)
	{
		num2.sign = 0;
		return subtract(num1, num2, bit, rounding);
	}

	int8_t sign = (num1.sign == 1 && num2.sign == 1) ? 1 : 0;
	int8_t remains_bits = 0;
	int8_t last_bit = 0;
	make_equal(&num1, &num2, &remains_bits);
	floatNum res = { sign, (uint64_t)((uint64_t)num1.mantissa + (uint64_t)num2.mantissa), num1.exponent };
	normalizeMantissa(&res, bit, &remains_bits, &last_bit);
	roundTo(&res, rounding, remains_bits, last_bit);
	specialNum(&res, bit);
	return res;
}

floatNum subtract(floatNum num1, floatNum num2, int8_t bit, int rounding)
{
	specialNum(&num1, bit);
	specialNum(&num2, bit);
	floatNum temp;
	special check = checkForSpecialAddSubtract(&temp, &num1, &num2);
	if (check)
	{
		return temp;
	}
	if (num2.sign == 1 && num1.sign == 0)
	{
		num2.sign = 0;
		return add(num2, num1, bit, rounding);
	}
	else if (num1.sign == 1 && num2.sign == 1)
	{
		num2.sign = 0;
		num1.sign = 0;
		return subtract(num2, num1, bit, rounding);
	}
	floatNum fl;
	int8_t remains_bits = 0;
	int8_t last_bit = 0;
	if (num1.exponent > (num1.exponent + bit))
	{
		fl.mantissa = num1.mantissa - 1;
		fl.exponent = num1.exponent;
	}
	else
	{
		make_equal(&num1, &num2, &remains_bits);
		fl.exponent = num1.exponent;
		if (num1.mantissa < num2.mantissa)
		{
			fl.sign = 1;
			fl.mantissa = (uint64_t)((uint64_t)num2.mantissa - (uint64_t)num1.mantissa);
		}
		else
		{
			fl.sign = num1.sign;
			fl.mantissa = (uint64_t)((uint64_t)num1.mantissa - (uint64_t)num2.mantissa);
		}
	}
	normalizeMantissa(&fl, bit, &remains_bits, &last_bit);
	roundTo(&fl, rounding, remains_bits, last_bit);
	specialNum(&fl, bit);
	return fl;
}

void printFloat(floatNum *res, int8_t bit)
{
	const char *formatStr = (bit == 23 ? "%06llxp%+d\n" : "%03llxp%+d\n");
	if (res->sign == 1)
		printf("-");
	if (res->special == infinity)
	{
		formatStr = "inf";
	}
	else if (res->special == nan)
	{
		formatStr = "nan";
	}
	else if (((res->mantissa - (1 << bit)) == 0 || res->mantissa == 0))
	{
		printf("0x0.");
		res->mantissa = 0;
		res->exponent = 0;
	}
	else
	{
		if (res->mantissa)
			res->mantissa -= (1 << (bit));
		res->mantissa &= (bit == 23 ? 0x007fffff : 0x3FF);
//		if (res->exponent == (bit == 23 ? -126 : -14))
//		{
//			printf("0x0.");
//		}
//		else
//		{
			printf("0x1.");
//		}
	}
	printf(formatStr, (res->mantissa) << (bit == 23 ? 1 : 2), res->exponent);
}

int main(int argc, char **argv)
{
	if (!(argc == 4 || argc == 6))
	{
		fprintf(stderr, "Wrong input! Not enough arguments");
		return ERROR_ARGUMENTS_INVALID;
	}
	int number;
	char bitchar;
	int8_t bit;
	int rounding;
	int error = 0;
	error += sscanf(argv[1], "%c", &bitchar);
	error += sscanf(argv[2], "%d", &rounding);
	if (rounding < 0 || rounding > 3)
	{
		fprintf(stderr, "Wrong input! Rounding parameter must be in [0, 3]");
		return ERROR_ARGUMENTS_INVALID;
	}
	error += sscanf(argv[3], "%i", &number);
	if (error != 3)
	{
		fprintf(stderr, "Wrong input! Program could not read the data");
		return ERROR_ARGUMENTS_INVALID;
	}
	switch (bitchar)
	{
	case 'f':
		bit = 23;
		break;
	case 'h':
		bit = 10;
		break;
	default:
		fprintf(stderr, "Wrong input! Unexpected precision: %c", bitchar);
		return ERROR_ARGUMENTS_INVALID;
	}
	if (argc == 4)
	{
		floatNum z1 = splitNum(number, bit);
		specialNum(&z1, bit);
		printFloat(&z1, bit);
	}
	else
	{
		char op;
		int number2;
		error += sscanf(argv[4], "%c", &op);
		error += sscanf(argv[5], "%i", &number2);
		floatNum r;
		if (error != 5)
		{
			fprintf(stderr, "Wrong input! Program could not read the data");
			return ERROR_ARGUMENTS_INVALID;
		}
		switch (op)
		{
		case '*':
		case 'M':
			r = mult(splitNum(number, bit), splitNum(number2, bit), bit, rounding);
			break;
		case '+':
			r = add(splitNum(number, bit), splitNum(number2, bit), bit, rounding);
			break;
		case '-':
			r = subtract(splitNum(number, bit), splitNum(number2, bit), bit, rounding);
			break;
		case '/':
			r = divide(splitNum(number, bit), splitNum(number2, bit), bit, rounding);
			break;
		default:
			fprintf(stderr, "Wrong input! Unexpected operator: %c", op);
			return ERROR_ARGUMENTS_INVALID;
		}
		printFloat(&r, bit);
	}
	return SUCCESS;
}
