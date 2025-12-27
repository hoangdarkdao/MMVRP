#include <bits/stdc++.h>

using namespace std;

// Cấu trúc dữ liệu
struct Customer {
    int id;
    double x, y;
    int demand;
};

struct Route {
    vector<int> nodes;
    int load = 0;
    double length = 0;
};

// Global variables
int N, K, Capacity;
vector<Customer> customers;
double distMatrix[1000][1000];

// --- HELPERS ---
double calcDist(int a, int b) {
    return sqrt(pow(customers[a].x - customers[b].x, 2) + pow(customers[a].y - customers[b].y, 2));
}

double calculateRouteLength(const vector<int>& nodes) {
    if (nodes.empty()) return 0;
    double len = distMatrix[0][nodes[0]];
    for (size_t i = 0; i < nodes.size() - 1; i++) {
        len += distMatrix[nodes[i]][nodes[i+1]];
    }
    len += distMatrix[nodes.back()][0];
    return len;
}

// Objective: Ưu tiên giảm Max Length, sau đó là tổng Length
double getObjective(const vector<Route>& routes) {
    double maxLen = 0;
    double totalLen = 0;
    for (const auto& r : routes) {
        maxLen = max(maxLen, r.length);
        totalLen += r.length;
    }
    return maxLen + 0.0001 * totalLen; 
}

// --- DESTROY OPERATORS ---

// 1. Random Removal
vector<int> randomRemove(vector<Route>& routes, int q) {
    vector<int> removed;
    for (int i = 0; i < q; i++) {
        int rIdx = rand() % K;
        if (routes[rIdx].nodes.empty()) continue;
        int nIdx = rand() % routes[rIdx].nodes.size();
        removed.push_back(routes[rIdx].nodes[nIdx]);
        routes[rIdx].nodes.erase(routes[rIdx].nodes.begin() + nIdx);
    }
    return removed;
}

// 2. Worst Removal (Xóa những node làm tăng chiều dài route nhiều nhất)
vector<int> worstRemove(vector<Route>& routes, int q) {
    vector<pair<double, pair<int, int>>> costs;
    for(int i=0; i<K; i++) {
        for(int j=0; j<routes[i].nodes.size(); j++) {
            int prev = (j == 0) ? 0 : routes[i].nodes[j-1];
            int curr = routes[i].nodes[j];
            int next = (j == routes[i].nodes.size()-1) ? 0 : routes[i].nodes[j+1];
            double cost = distMatrix[prev][curr] + distMatrix[curr][next] - distMatrix[prev][next];
            costs.push_back({cost, {i, j}});
        }
    }
    sort(costs.rbegin(), costs.rend());
    vector<int> removed;
    for(int i=0; i<min((int)costs.size(), q); i++) {
        int rIdx = costs[i].second.first;
        int val = routes[rIdx].nodes[costs[i].second.second];
        removed.push_back(val);
    }
    // Xóa node khỏi routes (cần cẩn thận index, ở đây đơn giản hóa bằng cách lọc lại)
    for(int id : removed) {
        for(int i=0; i<K; i++) {
            auto it = find(routes[i].nodes.begin(), routes[i].nodes.end(), id);
            if(it != routes[i].nodes.end()) {
                routes[i].nodes.erase(it);
                break;
            }
        }
    }
    return removed;
}

// --- REPAIR OPERATORS ---

// Greedy Insertion (như cũ nhưng tối ưu lại)
void greedyInsert(vector<Route>& routes, vector<int>& removed) {
    for (int custId : removed) {
        int bestVeh = -1, bestPos = -1;
        double minMaxAfter = 1e18;

        for (int v = 0; v < K; v++) {
            if (routes[v].load + customers[custId].demand <= Capacity) {
                for (int p = 0; p <= routes[v].nodes.size(); p++) {
                    vector<int> tempNodes = routes[v].nodes;
                    tempNodes.insert(tempNodes.begin() + p, custId);
                    double newLen = calculateRouteLength(tempNodes);
                    double currentMax = 0;
                    for(int i=0; i<K; i++) currentMax = max(currentMax, (i == v) ? newLen : routes[i].length);

                    if (currentMax < minMaxAfter) {
                        minMaxAfter = currentMax;
                        bestVeh = v; bestPos = p;
                    }
                }
            }
        }
        if (bestVeh != -1) {
            routes[bestVeh].nodes.insert(routes[bestVeh].nodes.begin() + bestPos, custId);
            routes[bestVeh].load += customers[custId].demand;
            routes[bestVeh].length = calculateRouteLength(routes[bestVeh].nodes);
        }
    }
}

// --- MAIN ALNS LOGIC ---

int main() {
    srand(time(NULL));
    if(!(cin >> N >> K >> Capacity)) return 0;
    customers.resize(N);
    for (int i = 0; i < N; i++) {
        cin >> customers[i].x >> customers[i].y >> customers[i].demand;
        customers[i].id = i;
    }

    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            distMatrix[i][j] = calcDist(i, j);

    // Khởi tạo tham số ALNS
    vector<double> d_weights = {1.0, 1.0}; // Trọng số cho Random và Worst removal
    double temperature = 100.0;
    double cooling_rate = 0.9995;
    
    // Khởi tạo lời giải ban đầu (Greedy)
    vector<Route> currentRoutes(K);
    vector<int> all_nodes;
    for(int i=1; i<N; i++) all_nodes.push_back(i);
    greedyInsert(currentRoutes, all_nodes);

    vector<Route> bestRoutes = currentRoutes;
    double bestObj = getObjective(bestRoutes);
    double currentObj = bestObj;

    auto startTime = chrono::steady_clock::now();
    int iter = 0;

    while (chrono::duration_cast<chrono::seconds>(chrono::steady_clock::now() - startTime).count() < 100) {
        vector<Route> tempRoutes = currentRoutes;
        int q = rand() % (N/5) + 2; // Số node xóa ngẫu nhiên từ 2 đến 20% N

        // 1. Chọn toán tử (Roulette Wheel đơn giản)
        vector<int> removed;
        int d_type = rand() % 2;
        if(d_type == 0) removed = randomRemove(tempRoutes, q);
        else removed = worstRemove(tempRoutes, q);

        // Update loads
        for(int i=0; i<K; i++) {
            tempRoutes[i].load = 0;
            for(int n : tempRoutes[i].nodes) tempRoutes[i].load += customers[n].demand;
            tempRoutes[i].length = calculateRouteLength(tempRoutes[i].nodes);
        }

        // 2. Repair
        greedyInsert(tempRoutes, removed);
        double tempObj = getObjective(tempRoutes);

        // 3. Acceptance (Simulated Annealing)
        double diff = tempObj - currentObj;
        if (diff < 0 || (exp(-diff / temperature) > (double)rand()/RAND_MAX)) {
            currentRoutes = tempRoutes;
            currentObj = tempObj;
            
            if (currentObj < bestObj) {
                bestObj = currentObj;
                bestRoutes = currentRoutes;
                cout << "Iteration " << iter << " | New Best: " << bestObj << endl;
            }
        }

        temperature *= cooling_rate;
        iter++;
    }

    // Output kết quả
    cout << "\n--- FINAL BEST SOLUTION ---" << endl;
    for (int i = 0; i < K; i++) {
        cout << "Vehicle " << i + 1 << " [Load " << bestRoutes[i].load << "]: 0 -> ";
        for (int n : bestRoutes[i].nodes) cout << n << " -> ";
        cout << "0 | Length: " << calculateRouteLength(bestRoutes[i].nodes) << endl;
    }

    return 0;
}