#include <bits/stdc++.h>

using namespace std;

struct Customer {
    double x, y;
    int demand;
};

struct Route {
    vector<int> nodes; // Chỉ chứa các khách hàng, mặc định bắt đầu và kết thúc tại 0
    double length = 0;
    int load = 0;
};

// --- BIẾN TOÀN CỤC ---
int N, K, Capacity;
vector<Customer> customers;
double distMatrix[1000][1000];

// --- HÀM HỖ TRỢ ---
double calculateDist(Customer c1, Customer c2) {
    return sqrt(pow(c1.x - c2.x, 2) + pow(c1.y - c2.y, 2));
}

double calculateRouteLength(const vector<int>& nodes) {
    if (nodes.empty()) return 0;
    double len = distMatrix[0][nodes[0]]; // Từ depot đến khách đầu tiên
    for (size_t i = 0; i < nodes.size() - 1; i++) {
        len += distMatrix[nodes[i]][nodes[i + 1]];
    }
    len += distMatrix[nodes.back()][0]; // Quay về depot
    return len;
}

double get_max_length(const vector<Route>& routes) {
    double maxL = 0;
    for (const auto& r : routes) maxL = max(maxL, r.length);
    return maxL;
}

// --- LOCAL SEARCH OPERATORS ---

// 1. 2-Opt nội bộ route
bool twoOpt(Route& r) {
    bool improved = false;
    if (r.nodes.size() < 2) return false;
    
    for (int i = 0; i < (int)r.nodes.size() - 1; i++) {
        for (int j = i + 1; j < (int)r.nodes.size(); j++) {
            vector<int> newNodes = r.nodes;
            reverse(newNodes.begin() + i, newNodes.begin() + j + 1);
            double newLen = calculateRouteLength(newNodes);
            if (newLen < r.length - 1e-6) {
                r.nodes = newNodes;
                r.length = newLen;
                improved = true;
            }
        }
    }
    return improved;
}

// 2. Relocate giữa các route (Chuyển 1 khách hàng từ route dài nhất sang route khác)
bool relocate(vector<Route>& routes) {
    int maxIdx = 0;
    for(int i=1; i<K; i++) if(routes[i].length > routes[maxIdx].length) maxIdx = i;

    for (int i = 0; i < (int)routes[maxIdx].nodes.size(); i++) {
        int cust = routes[maxIdx].nodes[i];
        for (int v = 0; v < K; v++) {
            if (v == maxIdx) continue;
            if (routes[v].load + customers[cust].demand <= Capacity) {
                for (int pos = 0; pos <= (int)routes[v].nodes.size(); pos++) {
                    vector<int> nextVNodes = routes[v].nodes;
                    nextVNodes.insert(nextVNodes.begin() + pos, cust);
                    double nextVLen = calculateRouteLength(nextVNodes);
                    
                    vector<int> nextMaxNodes = routes[maxIdx].nodes;
                    nextMaxNodes.erase(nextMaxNodes.begin() + i);
                    double nextMaxLen = calculateRouteLength(nextMaxNodes);

                    if (max(nextVLen, nextMaxLen) < get_max_length(routes) - 1e-6) {
                        routes[v].nodes = nextVNodes;
                        routes[v].length = nextVLen;
                        routes[v].load += customers[cust].demand;
                        
                        routes[maxIdx].nodes = nextMaxNodes;
                        routes[maxIdx].length = nextMaxLen;
                        routes[maxIdx].load -= customers[cust].demand;
                        return true;
                    }
                }
            }
        }
    }
    return false;
}

// 3. Two-opt move: Đảo ngược đoạn lộ trình
bool twoOptMove(Route& r) {
    if (r.nodes.size() < 2) return false;
    double oldLen = r.length;
    for (int i = 0; i < (int)r.nodes.size() - 1; i++) {
        for (int j = i + 1; j < (int)r.nodes.size(); j++) {
            reverse(r.nodes.begin() + i, r.nodes.begin() + j + 1);
            double newLen = calculateRouteLength(r.nodes);
            if (newLen < oldLen - 1e-6) { r.length = newLen; return true; }
            reverse(r.nodes.begin() + i, r.nodes.begin() + j + 1);
        }
    }
    return false;
}

// 4. Or-opt move: Di chuyển chuỗi 2-3 nút sang vị trí mới
bool orOptMove(Route& r) {
    for (int len = 2; len <= 3; len++) {
        if ((int)r.nodes.size() < len + 1) continue;
        for (int i = 0; i <= (int)r.nodes.size() - len; i++) {
            vector<int> segment(r.nodes.begin() + i, r.nodes.begin() + i + len);
            vector<int> remaining = r.nodes;
            remaining.erase(remaining.begin() + i, remaining.begin() + i + len);
            for (int pos = 0; pos <= (int)remaining.size(); pos++) {
                vector<int> test = remaining;
                test.insert(test.begin() + pos, segment.begin(), segment.end());
                double newL = calculateRouteLength(test);
                if (newL < r.length - 1e-6) { r.nodes = test; r.length = newL; return true; }
            }
        }
    }
    return false;
}

// 7. Cross-exchange Move: Trao đổi đoạn [i, j] của route r1 với đoạn [k, l] của route r2
bool crossExchangeMove(vector<Route>& routes) {
    double currentMax = get_max_length(routes);

    for (int r1 = 0; r1 < K; r1++) {
        for (int r2 = 0; r2 < K; r2++) {
            if (r1 == r2) continue; // Chỉ thực hiện giữa 2 route khác nhau

            int n1 = routes[r1].nodes.size();
            int n2 = routes[r2].nodes.size();

            // i, j là điểm bắt đầu và kết thúc đoạn trên route r1
            for (int i = 0; i < n1; i++) {
                for (int j = i; j < n1; j++) {
                    // k, l là điểm bắt đầu và kết thúc đoạn trên route r2
                    for (int k = 0; k < n2; k++) {
                        for (int l = k; l < n2; l++) {
                            
                            // 1. Tính toán tải trọng mới
                            int segment1Load = 0;
                            for (int idx = i; idx <= j; idx++) segment1Load += customers[routes[r1].nodes[idx]].demand;
                            
                            int segment2Load = 0;
                            for (int idx = k; idx <= l; idx++) segment2Load += customers[routes[r2].nodes[idx]].demand;

                            int newLoad1 = routes[r1].load - segment1Load + segment2Load;
                            int newLoad2 = routes[r2].load - segment2Load + segment1Load;

                            if (newLoad1 > Capacity || newLoad2 > Capacity) continue;

                            // 2. Tạo lộ trình mới tạm thời
                            vector<int> newNodes1, newNodes2;
                            
                            // Build newNodes1: [0...i-1] + [segment2] + [j+1...n1-1]
                            for (int x = 0; x < i; x++) newNodes1.push_back(routes[r1].nodes[x]);
                            for (int x = k; x <= l; x++) newNodes1.push_back(routes[r2].nodes[x]);
                            for (int x = j + 1; x < n1; x++) newNodes1.push_back(routes[r1].nodes[x]);

                            // Build newNodes2: [0...k-1] + [segment1] + [l+1...n2-1]
                            for (int x = 0; x < k; x++) newNodes2.push_back(routes[r2].nodes[x]);
                            for (int x = i; x <= j; x++) newNodes2.push_back(routes[r1].nodes[x]);
                            for (int x = l + 1; x < n2; x++) newNodes2.push_back(routes[r2].nodes[x]);

                            double len1 = calculateRouteLength(newNodes1);
                            double len2 = calculateRouteLength(newNodes2);

                            // 3. Kiểm tra mục tiêu Min-Max
                            // Lưu trạng thái cũ để so sánh
                            double oldLen1 = routes[r1].length;
                            double oldLen2 = routes[r2].length;
                            
                            routes[r1].length = len1;
                            routes[r2].length = len2;
                            
                            double newMax = get_max_length(routes);

                            if (newMax < currentMax - 1e-6) {
                                // Chấp nhận thay đổi
                                routes[r1].nodes = newNodes1;
                                routes[r1].load = newLoad1;
                                routes[r2].nodes = newNodes2;
                                routes[r2].load = newLoad2;
                                return true;
                            } else {
                                // Khôi phục lại (Rollback)
                                routes[r1].length = oldLen1;
                                routes[r2].length = oldLen2;
                            }
                        }
                    }
                }
            }
        }
    }
    return false;
}

// --- THUẬT TOÁN CHÍNH ---

int main() {
    // Nhập dữ liệu
    cin >> N >> K >> Capacity;
    customers.resize(N);
    for (int i = 0; i < N; i++) {
        cin >> customers[i].x >> customers[i].y >> customers[i].demand;
    }

    // Khởi tạo ma trận khoảng cách
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            distMatrix[i][j] = calculateDist(customers[i], customers[j]);
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
                    int lastNode = currentRoutes[v].nodes.empty() ? 0 : currentRoutes[v].nodes.back();
                    double d = distMatrix[lastNode][c];
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

    // --- RECORD-TO-RECORD (RTR) ---
    vector<Route> bestSolution = currentRoutes;
    double record = get_max_length(bestSolution);
    double deviation = 0.05; // Ngưỡng chấp nhận giải pháp tệ hơn một chút để thoát tối ưu cục bộ
	
	srand(time(NULL));
    auto startTime = chrono::steady_clock::now();
    
    while (chrono::duration_cast<chrono::milliseconds>(chrono::steady_clock::now() - startTime).count() < 100000) {
        bool improved = false;

        // Áp dụng Local Search nội bộ mỗi route
        for (int i = 0; i < K; i++) {
            if (twoOpt(currentRoutes[i])) improved = true;
            if (orOptMove(currentRoutes[i])) improved = true;
        }

        // Áp dụng Inter-route Local Search (Relocate)
        if (relocate(currentRoutes)) improved = true;
		
		if (crossExchangeMove(currentRoutes)) improved = true;
		
        double currentMax = get_max_length(currentRoutes);
        
        // Cập nhật Record
        if (currentMax < record - 1e-6) {
        	cout<<record<<endl;
            record = currentMax;
            bestSolution = currentRoutes;
            improved = true;
        } 
        // Cơ chế RTR: Chấp nhận giải pháp nếu không vượt quá record + ngưỡng
        else if (currentMax > record + record * deviation) {
            currentRoutes = bestSolution; // Quay lại nếu quá tệ
        }

        //if (!improved && iter > 200) break; 
    }

    // --- XUẤT KẾT QUẢ ---
    cout << fixed << setprecision(2);
    cout << "Max Route Length (Min-Max): " << record << endl;
    for (int i = 0; i < K; i++) {
        cout << "Xe " << i + 1 << " (Load: " << bestSolution[i].load << "): 0 -> ";
        for (int node : bestSolution[i].nodes) cout << node << " -> ";
        cout << "0 | Length: " << bestSolution[i].length << endl;
    }

    return 0;
}