#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <fstream>

using namespace std;

typedef vector<string> Lines;
typedef int (*Fold_func) (const string &);

class Tool {
public:
    Tool() {
        //cout << "Tool init." << endl;
    };

    ~Tool() {};

    static Lines input(int day) {
        Lines lines;
        stringstream filename;

        filename << "inputs/" << "d" << day << ".txt";
        cout << filename.str() << endl;

        string str;
        fstream inputFile(filename.str(), fstream::in);
        while (! inputFile.eof()) {
            getline(inputFile, str);
            lines.push_back(str);
        }

        return lines;
    }

    template<typename T>
    static void dump_lines(vector<T>& lines, int max = -1)
    {
        int count = lines.size();

        if (max > 0) {
            count = max;
        }

        for (const auto& line : lines) {
            cout << line << endl;
            if (--count == 0) {
                break;
            }
        }
    }

    static void dump_vector_string(const char* name, vector<string>& lines)
    {
        cout << name << ": " << lines.size() << endl;
        for (const auto& line : lines) {
            cout << line << endl;
        }
    }

    template<typename T1>
    static int fold_vector(vector<T1>& items, int (*fold_func) (const T1 &))
    {
        int count = 0;

        for (const auto& item : items) {
            count += fold_func(item);
        }

        return count;
    }

    static vector<string> string_split(const string& str, const string &delim)
    {
        vector<string> tokens;
        size_t start = 0;
        size_t pos;
        size_t str_len = str.length();
        size_t delim_len = delim.length();

        pos = str.find(delim);
        while (pos != string::npos) {
            // Leading delim
            if (pos == start) {
                start = delim_len;
                continue;
            }

            //cout << "Found at " << start << ", " << pos << ", " << str << endl;
            tokens.push_back(str.substr(start, pos - start));
            start = pos + delim_len;
            pos = str.find(delim, start);
        }

        if (start < (str_len - 1)) {
            tokens.push_back(str.substr(start, str_len - start));
        }

        return tokens;
    }
};