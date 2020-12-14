#include <string>
#include <iostream>
#include <algorithm>
#include <vector>
#include "tool.cpp"

using namespace std;

static Tool T;

class Bus {
public:
    int id;
    int period;
    int distance;
    int loop_start;
    int loop_n;
    int start_offset;
};

ostream& operator<< (ostream& os, const Bus& bus) {
    os << bus.id << " " << bus.period;
    return os;
};

static int first_timestamp = 0;
static vector<Bus> buses;

static void parse_buses(const string& line)
{
    int id = 0;
    vector<string> tokens = T.string_split(line, ",");

    for (const auto& str: tokens) {
        Bus bus = { id, -1, INT32_MAX, 0, 1, 0 };
        if (str != "x") {
            bus.period = stoi(str);
            bus.distance = bus.period - (first_timestamp % bus.period);
        }
        buses.push_back(bus);
        id++;
    }

    //T.dump_lines(buses);
}

static bool bus_sort_by_distance(Bus& a, Bus& b)
{
    return a.distance < b.distance;
}

static bool bus_sort_by_id(Bus& a, Bus& b)
{
    return a.id < b.id;
}

static Bus & find_nearest_bus()
{
    sort(buses.begin(), buses.end(), bus_sort_by_distance);
    return buses[0];
}

static long gcd(long a, long b) 
{
    if (a == 0) return b;
    if (b == 0) return a;
    if (a > b)
        return gcd (b, a % b);
    else
        return gcd (a, b % a);
}

static long lcm(long a, long b) 
{
    return (a*b) / gcd(a, b);
}

static long bus_lcm(Bus & bus1, Bus & bus2)
{

    int b1 = bus1.period;
    int b2 = bus2.period;
    int offset_target = bus2.id - bus1.id;
    int f1 = 1;
    int found = 0;

    while (1) {
        int offset = (f1 * b2) % b1;
        //cout << "M " << f1 << ", Offset " << offset << endl;
        if (offset == offset_target) {
            found++;
            //cout << "Found" << endl;
            if (found == 1) {
                bus2.loop_start = f1;
            } else {
                bus2.loop_n = f1 - bus2.loop_start;
                break;
            }
        }
        f1++;
    }

    cout << b1 << ", " << b2 << ", m1 " << f1 * b2 - offset_target;
    cout << ", m2 " << f1 * b2;
    cout << ", loop_start " << bus2.loop_start;
    cout << ", loop_n " << bus2.loop_n << endl;

    return 0;
}

static long long find_first_lcm()
{
    vector<Bus> valid_buses;
    long long fisrt_lcm = 0;

    sort(buses.begin(), buses.end(), bus_sort_by_id);

    for (auto& bus : buses) {
        if (bus.period > 0) {
            valid_buses.push_back(bus);
        }
    }

    Bus previous_bus = { -1, 0, 0, 0 , 0};

    for (auto& bus : valid_buses) {
        if (previous_bus.id == -1) {
            previous_bus =  bus;
            continue;
        }
        bus_lcm(previous_bus, bus);
        previous_bus = bus;
    }

    long llcm = 1;
    for (int i = 1; i < valid_buses.size(); i++) {
        llcm = lcm(llcm, valid_buses[i].loop_n);
    }
    cout << "LCM " << llcm << endl;

    return fisrt_lcm;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(13, -2);
	//T.dump_lines(lines, 10);

    first_timestamp = stoi(lines[0]);
    cout << "First time " << first_timestamp << endl;

    parse_buses(lines[1]);

    cout << "Part 1" << endl;
    Bus & bus = find_nearest_bus();
    cout << "Nearest bus " << bus.period << ", mults " << bus.period * bus.distance << endl;

    //find_first_lcm();

	return 0;
}