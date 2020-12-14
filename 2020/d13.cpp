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
    int position;
    int period;
    int distance;
    int loop_start;
    int loop_n;
    int loop_step;
    int start_offset;
    int id_offset;
};

ostream& operator<< (ostream& os, const Bus& bus) {
    os << bus.id;
    os << " Pos " << bus.position;
    os << " Period " << bus.period;
    os << " Loop step " << bus.loop_step;
    return os;
};

static int first_timestamp = 0;
static vector<Bus> buses;

static void parse_buses(const string& line)
{
    int id = 0;
    vector<string> tokens = T.string_split(line, ",");

    for (const auto& str: tokens) {
        Bus bus = { id, id, -1, INT32_MAX, 0, 1, 0, 0, 0 };
        if (str != "x") {
            bus.period = stoi(str);
            bus.loop_step = bus.period;
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

static bool bus_sort_by_loop_step_dec(Bus& a, Bus& b)
{
    return a.loop_step > b.loop_step;
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
    int offset_target = bus2.position - bus1.position;
    int step = 1;
    int point = 0;
    int found = 0;

    while (1) {
        point += bus2.loop_step;
        int offset = point % b1;
        //cout << "M " << step << ", Offset " << offset;
        //cout << ", Target " << offset_target << endl;
        if (offset == offset_target) {
            found++;
            //cout << "Found" << endl;
            if (found == 1) {
                bus2.loop_start = step;
                bus2.start_offset = bus2.loop_start * bus2.period;
            } else {
                bus2.loop_n = step - bus2.loop_start;
                bus2.loop_step = bus2.loop_n * bus2.period;
                break;
            }
        }
        step++;
    }

    cout << b1 << ", " << b2 << ", offset " << offset_target;
    cout << ", m1 " << step * b2 - offset_target;
    cout << ", m2 " << step * b2;
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

    T.dump_lines(valid_buses);

    for (int i = 1; i < valid_buses.size(); i++) {
        bus_lcm(valid_buses[i - 1], valid_buses[i]);
    }

 // part 2 draft, works with d13-2
 #if 1
    sort(valid_buses.begin(), valid_buses.end(), bus_sort_by_loop_step_dec);
    T.dump_lines(valid_buses);

    Bus busmx = valid_buses[0];
    valid_buses.erase(valid_buses.begin());

    for (auto& bus: valid_buses) {
        bus.id_offset = busmx.id - bus.id;
    }

    long start_point = busmx.start_offset;
    long step = busmx.loop_step;
    long count = 1;

    while (1) {
        bool found = true;
        start_point += step;
        cout << "Try " << start_point << ", step " << count++ << endl;
        for (const auto& bus: valid_buses) {
            if (bus.id_offset > 0) {
                if (! (bus.id_offset == (start_point % bus.period))) {
                    found = false;
                    break;
                }
            } else {
                if (! (0 == (start_point - bus.id_offset) % bus.period)) {
                    found = false;
                    break;

                }
            }
        }
        if (found) {
            cout << "Found " << start_point - (busmx.id - buses[0].id) << endl;
            break;
        }
    }
#endif

#if 0
    long llcm = 1;
    for (int i = 1; i < valid_buses.size(); i++) {
        llcm = lcm(llcm, valid_buses[i].loop_n);
    }
    cout << "LCM " << llcm << endl;
#endif

    return fisrt_lcm;
}

int main (int argc, char *argv[])
{
	Lines lines = T.input(13, +2);
	//T.dump_lines(lines, 10);

    first_timestamp = stoi(lines[0]);
    cout << "First time " << first_timestamp << endl;

    parse_buses(lines[1]);

    cout << "Part 1" << endl;
    Bus & bus = find_nearest_bus();
    cout << "Nearest bus " << bus.period << ", mults " << bus.period * bus.distance << endl;

    find_first_lcm();

	return 0;
}