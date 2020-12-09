#include <string>
#include <iostream>
#include <vector>
#include <map>
#include "tool.cpp"

using namespace std;

static Tool T;

typedef struct Bag Bag;
typedef map<string, Bag> BagHash;

struct Bag {
    int         count;
    string      name;
    vector<Bag> sub_bags;
};

static BagHash bags_hash;
static vector<Bag> bags;

static vector<Bag> parse_sub_bags(Bag& bag, string& line)
{
    vector<Bag> sub_bags;
    string bag_delim = ", ";
    string SPACE = " ";
    vector<string> bags_str;

    bags_str = T.string_split(line, bag_delim);
    //T.dump_vector_string("Bags found", bags_str);

    for (const auto& bag_str : bags_str) {
        vector<string> bag_info = T.string_split(bag_str, SPACE);
        Bag bag;

        if (bag_info.size() >= 3 and bag_info[0] != "no") {
            //T.dump_vector_string("Bag token", bag_info);
            bag.count = stoi(bag_info[0]);
            bag.name = bag_info[1] + " " + bag_info[2];
            sub_bags.push_back(bag);
        }
    }

    return sub_bags;
}

// clear purple bags contain 5 faded indigo bags, 3 muted purple bags.
const static string bags_line_separater = "bags contain ";

static int parse_line(const string& line)
{
    size_t pos;
    string str;
    Bag bag;

    pos = line.find(bags_line_separater);
    if (pos != string::npos) {
        bag.count = 1;
        bag.name = line.substr(0, pos - 1);
        str = line.substr(pos + bags_line_separater.length());
        bag.sub_bags = parse_sub_bags(bag, str);
        bags_hash.insert(pair<string, Bag>(bag.name, bag));
        bags.push_back(bag);
    }

    return 0;
}

static int bag_hold_shiny_god(const Bag& bag)
{
    for (const auto& sub_bag : bag.sub_bags) {
        //cout << "Check bag " << bag.name << ", sub bag " << sub_bag.name << endl;
        if (sub_bag.name.compare("shiny gold") == 0)
            return 1;
        
        BagHash::iterator biter =  bags_hash.find(sub_bag.name);
        if (biter != bags_hash.end()) {
            int ret = bag_hold_shiny_god(biter->second);
            if (ret == 1) {
                return 1;
            }
        }
    }

    return 0;
}

static int bags_in_bag(int n, const char* name)
{
    int count = 0;
    BagHash::iterator biter =  bags_hash.find(name);

    if (biter != bags_hash.end()) {
        Bag& bag = biter->second;

        for (const auto& sub_bag : bag.sub_bags) {
            count += n * sub_bag.count;
            count += n * bags_in_bag(sub_bag.count, sub_bag.name.c_str());
        }
    }

    return count;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(7);
	//T.dump_lines(lines, 10);

    T.fold_vector(lines, parse_line);
    cout << "Bags num " << bags.size() << endl;

    cout << "Bags num of hold shiny gold " << T.fold_vector(bags, bag_hold_shiny_god) << endl;
    cout << "Bags in shiny gold bag " << bags_in_bag(1, "shiny gold") << endl;

	return 0;
}