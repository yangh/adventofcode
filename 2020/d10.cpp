#include <string>
#include <iostream>
#include <algorithm>
#include <vector>
#include <math.h>
#include "tool.cpp"

using namespace std;

static Tool T;
static Numbers numbers;

static int multi_jolts_1_3()
{
	int j1 = 0;
	int j3 = 0;
	long last = 0;

	for(const auto& num : numbers) {
		long offset = num - last;
		if (offset == 1) {
			j1++;
		} else if (offset == 3) {
			j3++;
		} else {
			cerr << "Unknown offset " << offset << " " << last << " " << num << endl;
		}
		last = num;
	}

	// One extra jolt = max + 3;
	return j1 * (j3 + 1);
}

static long composite_jolts()
{
	int j1 = 0;
	long last = 0;
	long compose = 1;

	for(int i = 0; i < numbers.size(); i++) {
		j1 = 0;
		int j = 0;

		//for (j = i + 1; j < numbers.size() and j < i + 4; j++) {
		for (j = i + 1; j < numbers.size(); j++) {
			long n1 = numbers[j-1];
			long n2 = numbers[j];
			long offset = n2 - n1;
			//cout << "Check " << n1 << " " << n2 << " " << offset << endl;
			if (offset == 1) {
				j1++;
			} else {
				break;
			}
		}

		if (j1 > 1) {
			if (j1 == 2) {
				compose *= 2;
			} else if (j1 == 3) {
				compose *= 4;
			} else if (j1 == 4) {
				compose *= 7;
			}
			cout << "J " << j1 << ", at " << i << ", compose " << compose << endl;

			i += j1;
		}
	}

	return compose;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(10, -2);

	T.lines_to_numbers(lines, numbers);
	sort(numbers.begin(), numbers.end());
	//T.dump_lines(numbers, 32);

	numbers.insert(numbers.begin(), 0);
	cout << "Multiple of j1 j3 " << multi_jolts_1_3() << endl;

	// TODO: Finsh part 2
	long comp = composite_jolts();
	cout << "Composites of jolts " << comp << endl;

	return 0;
}