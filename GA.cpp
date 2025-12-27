#include <bits/stdc++.h>

using namespace std;

struct Point {
    double x, y;
    int demand;
};

struct Individual {
    vector<vector<int>> routes; 
    double max_len;             
    
    // Sắp xếp để cá thể có max_len nhỏ hơn (tốt hơn) đứng trước
    bool operator<(const Individual& other) const {
        return max_len < other.max_len;
    }
};

int N, K, Capacity;
vector<Point> pts;
double distMat[5001][5001];

double get_dist(int i, int j) { return distMat[i][j]; }

// Hàm tính chiều dài 1 lộ trình (bao gồm cả quay về depot)
double calc_route_len(const vector<int>& r) {
    if (r.empty()) return 0;
    double d = get_dist(0, r[0]);
    for (size_t i = 0; i < r.size() - 1; i++) {
        d += get_dist(r[i], r[i+1]);
    }
    d += get_dist(r.back(), 0);
    return d;
}

// Hàm Local Search 2-opt cho GA
void twoOpt(vector<int>& route) {
    if (route.size() < 2) return;
    bool improved = true;
    while (improved) {
        improved = false;
        for (int i = 0; i < (int)route.size() - 1; i++) {
            for (int j = i + 1; j < (int)route.size(); j++) {
                int a = (i == 0) ? 0 : route[i-1];
                int b = route[i];
                int c = route[j];
                int d = (j == (int)route.size() - 1) ? 0 : route[j+1];

                double curD = get_dist(a, b) + get_dist(c, d);
                double newD = get_dist(a, c) + get_dist(b, d);

                if (newD < curD - 1e-7) {
                    reverse(route.begin() + i, route.begin() + j + 1);
                    improved = true;
                }
            }
        }
    }
}

// Hàm khởi tạo cá thể theo yêu cầu của bạn
Individual create_individual(bool is_pure_greedy) {
    Individual ind;
    ind.routes.resize(K);
    vector<int> loads(K, 0);
    vector<int> unassigned;
    for(int i = 1; i < N; i++) unassigned.push_back(i);

    while (!unassigned.empty()) {
        int bestIdx = -1, bestV = -1;
        double minD = 1e18;

        if (is_pure_greedy) {
            // Greedy xịn: Tìm khách hàng gần nhất với điểm cuối của bất kỳ xe nào
            for (int i = 0; i < (int)unassigned.size(); i++) {
                int c = unassigned[i];
                for (int v = 0; v < K; v++) {
                    if (loads[v] + pts[c].demand <= Capacity) {
                        int last = ind.routes[v].empty() ? 0 : ind.routes[v].back();
                        double d = get_dist(last, c);
                        if (d < minD) {
                            minD = d;
                            bestIdx = i;
                            bestV = v;
                        }
                    }
                }
            }
        } else {
            // Random Greedy: Chọn khách ngẫu nhiên, cho vào xe có khoảng cách tăng thêm ít nhất
            bestIdx = rand() % unassigned.size();
            int c = unassigned[bestIdx];
            for (int v = 0; v < K; v++) {
                if (loads[v] + pts[c].demand <= Capacity) {
                    int last = ind.routes[v].empty() ? 0 : ind.routes[v].back();
                    double d = get_dist(last, c);
                    if (d < minD) {
                        minD = d;
                        bestV = v;
                    }
                }
            }
        }

        if (bestV != -1) {
            ind.routes[bestV].push_back(unassigned[bestIdx]);
            loads[bestV] += pts[unassigned[bestIdx]].demand;
            unassigned.erase(unassigned.begin() + bestIdx);
        } else {
            break; // Hết xe có thể chứa
        }
    }

    // Sửa lỗi: Gọi twoOpt cho từng vector<int> trong routes
    ind.max_len = 0;
    for (auto& r : ind.routes) {
        twoOpt(r);
        ind.max_len = max(ind.max_len, calc_route_len(r));
    }
    return ind;
}

// Hàm đột biến đơn giản: Đổi chỗ khách hàng giữa 2 xe
void mutate(Individual& ind) {
    int v1 = rand() % K;
    int v2 = rand() % K;
    if (ind.routes[v1].empty()) return;

    int idx = rand() % ind.routes[v1].size();
    int cust = ind.routes[v1][idx];

    // Tính tải trọng hiện tại v2
    int l2 = 0;
    for(int c : ind.routes[v2]) l2 += pts[c].demand;

    if (v1 != v2 && l2 + pts[cust].demand <= Capacity) {
        ind.routes[v1].erase(ind.routes[v1].begin() + idx);
        ind.routes[v2].push_back(cust);
        twoOpt(ind.routes[v1]);
        twoOpt(ind.routes[v2]);
        
        ind.max_len = 0;
        for (auto& r : ind.routes) ind.max_len = max(ind.max_len, calc_route_len(r));
    }
}

int main() {
    ios::sync_with_stdio(0); cin.tie(0);
    srand(time(NULL));

    if (!(cin >> N >> K >> Capacity)) return 0;
    pts.resize(N);
    for (int i = 0; i < N; i++) cin >> pts[i].x >> pts[i].y >> pts[i].demand;

    for (int i = 0; i < N; i++)
        for (int j = 0; j < N; j++)
            distMat[i][j] = sqrt(pow(pts[i].x-pts[j].x, 2) + pow(pts[i].y-pts[j].y, 2));

    int popSize = 30; // Với N=5000, popSize nên vừa phải để tránh chậm
    vector<Individual> population;

    // Khởi tạo: 20% Pure Greedy, 80% Random Greedy
    for (int i = 0; i < popSize; i++) {
        population.push_back(create_individual(i < popSize * 0.2));
    }

    // Tiến hóa
    for (int gen = 0; gen < 500; gen++) {
        sort(population.begin(), population.end());
        
        // Elite: giữ lại top 5
        vector<Individual> nextGen;
        for(int i=0; i<5; i++) nextGen.push_back(population[i]);

        // Tạo con bằng đột biến từ nhóm Elite
        while(nextGen.size() < popSize) {
            Individual child = population[rand() % 5];
            mutate(child);
            nextGen.push_back(child);
        }
        population = nextGen;
        if(gen % 10 == 0) cout << "Gen " << gen << " Best Max: " << population[0].max_len << endl;
    }

    sort(population.begin(), population.end());
    cout << "\n--- FINAL BEST (GA) ---\nMax Length: " << population[0].max_len << endl;
    for(int i=0; i<K; i++) {
        cout << "Route " << i+1 << ": 0 -> ";
        for(int c : population[0].routes[i]) cout << c << " -> ";
        cout << "0\n";
    }

    return 0;
}