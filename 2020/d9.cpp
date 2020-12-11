#include <string>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;
static Numbers numbers;

static bool is_a_preable_num(int idx)
{
    long n = numbers[idx];

    for (int i = idx - 25; i < idx; i++) {
        long n1 = numbers[i];
        for (int j = i + 1; j < idx; j++) {
            long n2 = numbers[j];
            if (n == (n1 + n2)) {
                return true;
            }
        }
    }

    return false;
}

static int find_first_invalid_num(int preable_len)
{
    for (int i = preable_len; i < numbers.size(); i++) {
        if (! is_a_preable_num(i)) {
            return i;
        }
    }

    return -1;
}

static bool find_sum_of_invalid_num(int idx, int sum_idx, int& end)
{
    long sum = numbers[sum_idx];

    for (int i = idx; i < sum_idx; i++) {
        sum -= numbers[i];

        if (sum == 0) {
            end = i;
            return true;
        } else if (sum < 0) {
            return false;
        }
    }

    return false;
}

static long sum_of_min_max_in_range(int start, int end)
{
    // Init to same number to ensure min/max works as design.
    long min = numbers[start];
    long max = numbers[start];

    for (int i = start; i <= end; i++) {
        if (numbers[i] < min) {
            min = numbers[i];
        } else if (numbers[i] > max) {
            max = numbers[i];
        }
    }

    return min + max;
}

static int find_continguous_sum_of_invalid_num(long invalid_num_idx)
{
    int end = -1;

    for (int i = 0; i < invalid_num_idx; i++) {
        if (find_sum_of_invalid_num(i, invalid_num_idx, end)) {
            return sum_of_min_max_in_range(i, end);
        }
    }

    return -1;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(9);

    T.lines_to_numbers(lines, numbers);

    long invalid_num_idx = find_first_invalid_num(25);
    long invalid_num = numbers[invalid_num_idx];
    cout << "First none-preable number " << invalid_num << endl;

    long sum = find_continguous_sum_of_invalid_num(invalid_num_idx);
    cout << "Sum of start/end continguous number " << sum << endl;

	return 0;
}