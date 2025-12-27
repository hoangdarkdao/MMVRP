#include <bits/stdc++.h>

using namespace std;

struct Point {
    double x, y;
    int demand;
};

struct Route {
    vector<int> nodes;
    double length;
    int load;

    Route() : length(0), load(0) {
        nodes.push_back(0); // Luôn bắt đầu tại Depot (ID 0)
    }
};

// Hàm tính khoảng cách Euclid
double dist(Point a, Point b) {
    return sqrt(pow(a.x - b.x, 2) + pow(a.y - b.y, 2));
}

// Khai báo ma trận khoảng cách toàn cục (Cẩn thận với bộ nhớ nếu N quá lớn)
double distMatrix[5001][5001];

double calculateRouteLength(const vector<int>& route) {
    if (route.size() <= 1) return 0;
    double d = 0;
    for (size_t i = 0; i < route.size() - 1; ++i) {
        d += distMatrix[route[i]][route[i+1]];
    }
    d += distMatrix[route.back()][0]; // Quay về depot
    return d;
}

// Local Search: 2-opt tối ưu nội bộ 1 xe
void twoOpt(vector<int>& route) {
    bool improved = true;
    while (improved) {
        improved = false;
        for (int i = 1; i < (int)route.size() - 1; i++) {
            for (int j = i + 1; j < (int)route.size(); j++) {
                // Tính toán sự thay đổi khoảng cách nếu đảo ngược đoạn [i, j]
                int prev = route[i-1];
                int curr = route[i];
                int next_j = (j + 1 == (int)route.size()) ? 0 : route[j+1];
                int end_j = route[j];

                double currentDist = distMatrix[prev][curr] + distMatrix[end_j][next_j];
                double newDist = distMatrix[prev][end_j] + distMatrix[curr][next_j];

                if (newDist < currentDist - 1e-9) {
                    reverse(route.begin() + i, route.begin() + j + 1);
                    improved = true;
                }
            }
        }
    }
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    int N, K, Capacity;
    if (!(cin >> N >> K >> Capacity)) return 0;

    vector<Point> customers(N);
    for (int i = 0; i < N; i++) {
        cin>> customers[i].x >> customers[i].y >> customers[i].demand;
    }

    // Tiền tính toán ma trận khoảng cách
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            distMatrix[i][j] = dist(customers[i], customers[j]);
        }
    }

    // --- KHỞI TẠO (GREEDY) ---
    vector<Route> currentRoutes(K);
    vector<bool> visited(N, false);
    visited[0] = true;

    for (int i = 1; i < N; i++) {
        int bestCust = -1, bestVeh = -1;
        double minAddedDist = 1e18;

        for (int v = 0; v < K; v++) {
            for (int c = 1; c < N; c++) {
                if (!visited[c] && currentRoutes[v].load + customers[c].demand <= Capacity) {
                    double d = distMatrix[currentRoutes[v].nodes.back()][c];
                    if (d < minAddedDist) {
                        minAddedDist = d;
                        bestCust = c;
                        bestVeh = v;
                    }
                }
            }
        }
        if (bestCust != -1) {
            currentRoutes[bestVeh].nodes.push_back(bestCust);
            currentRoutes[bestVeh].load += customers[bestCust].demand;
            visited[bestCust] = true;
        }
    }

    for (int i = 0; i < K; i++) currentRoutes[i].length = calculateRouteLength(currentRoutes[i].nodes);

    // Lưu kết quả tốt nhất
    vector<Route> bestSolution = currentRoutes;
    auto get_max = [](const vector<Route>& rs) {
        double m = 0;
        for (const auto& r : rs) m = max(m, r.length);
        return m;
    };
    double bestMaxLen = get_max(bestSolution);

    // --- ITERATED LOCAL SEARCH (ILS) ---
    srand(time(NULL));
    auto startTime = chrono::steady_clock::now();
    
    // Chạy trong khoảng 1.5 giây để đảm bảo an toàn thời gian
    while (chrono::duration_cast<chrono::milliseconds>(chrono::steady_clock::now() - startTime).count() < 150) {
        cout<< bestMaxLen<< endl;
        // Perturbation: Chuyển 1 node từ xe tệ nhất sang xe khác ngẫu nhiên
        int longestIdx = 0;
        for(int i=1; i<K; i++) if(currentRoutes[i].length > currentRoutes[longestIdx].length) longestIdx = i;

        if (currentRoutes[longestIdx].nodes.size() > 1) {
            int randIdx = 1 + rand() % (currentRoutes[longestIdx].nodes.size() - 1);
            int cust = currentRoutes[longestIdx].nodes[randIdx];
            int targetVeh = rand() % K;

            if (targetVeh != longestIdx && currentRoutes[targetVeh].load + customers[cust].demand <= Capacity) {
                // Di chuyển khách hàng
                currentRoutes[longestIdx].nodes.erase(currentRoutes[longestIdx].nodes.begin() + randIdx);
                currentRoutes[targetVeh].nodes.push_back(cust);

                // Local Search (2-opt) để tối ưu lại
                twoOpt(currentRoutes[longestIdx].nodes);
                twoOpt(currentRoutes[targetVeh].nodes);

                // Cập nhật thông số
                currentRoutes[longestIdx].load -= customers[cust].demand;
                currentRoutes[targetVeh].load += customers[cust].demand;
                currentRoutes[longestIdx].length = calculateRouteLength(currentRoutes[longestIdx].nodes);
                currentRoutes[targetVeh].length = calculateRouteLength(currentRoutes[targetVeh].nodes);

                double currentMax = get_max(currentRoutes);
                if (currentMax < bestMaxLen) {
                    bestMaxLen = currentMax;
                    bestSolution = currentRoutes;
                }
            }
        }
    }

    // --- IN KẾT QUẢ ---
    cout << "------------------------------------------" << endl;
    cout << "KET QUA TOI UU (MIN-MAX VRP):" << endl;
    cout << "Quang duong dai nhat (Makespan): " << fixed << setprecision(2) << bestMaxLen << endl;
    cout << "------------------------------------------" << endl;

    for (int i = 0; i < K; i++) {
        cout << "Xe #" << i + 1 << " [Tai trong: " << bestSolution[i].load << "/" << Capacity 
             << ", Quang duong: " << bestSolution[i].length << "]:" << endl;
        for (int node : bestSolution[i].nodes) {
            cout << node << " -> ";
        }
        cout << "0" << endl; // Quay về depot
        cout << endl;
    }

    return 0;
}