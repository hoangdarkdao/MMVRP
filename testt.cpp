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
double distMatrix[1010][1010];

// --- HÀM HỖ TRỢ ---
double calculateDist(Customer c1, Customer c2) {
    return sqrt(pow(c1.x - c2.x, 2) + pow(c1.y - c2.y, 2));
}

double calculateRouteLength(const vector<int>& nodes) {
    if (nodes.empty()) return 0.0;
    double len = distMatrix[0][nodes[0]];
    for (size_t i = 0; i + 1 < nodes.size(); i++) {
        len += distMatrix[nodes[i]][nodes[i + 1]];
    }
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
    if (r.nodes.size() < 3) return false;
    const int NUM_ATTEMPTS = 500;

    for (int attempt = 0; attempt < NUM_ATTEMPTS; attempt++) {
        int i = rand() % (r.nodes.size() - 1);
        int j = i + 1 + rand() % (r.nodes.size() - i - 1);

        vector<int> test = r.nodes;
        reverse(test.begin() + i, test.begin() + j + 1);
        double newLen = calculateRouteLength(test);

        if (newLen < r.length - 1e-6) {
            r.nodes = test;
            r.length = newLen;
            return true;
        }
    }
    return false;
}

// 2. Relocate giữa các route (Chuyển 1 khách hàng từ route dài nhất sang route khác)
bool relocate(vector<Route>& routes) {
    double currentMax = get_max_length(routes);
    
    // Thử nhiều lần ngẫu nhiên
    const int NUM_ATTEMPTS = 1000;  // Có thể điều chỉnh: 500-2000
    for (int attempt = 0; attempt < NUM_ATTEMPTS; attempt++) {
        // Chọn ngẫu nhiên một route (ưu tiên route dài hơn một chút)
        vector<pair<double, int>> routeScores;
        for (int i = 0; i < K; i++) {
            routeScores.emplace_back(routes[i].length, i);
        }
        // Có thể shuffle hoặc chọn theo xác suất tỷ lệ với độ dài

        // Chọn ngẫu nhiên route nguồn (source)
        int src = rand() % K;
        if (routes[src].nodes.empty()) continue;

        // Chọn ngẫu nhiên một khách trong route nguồn
        int posInSrc = rand() % routes[src].nodes.size();
        int cust = routes[src].nodes[posInSrc];

        // Chọn ngẫu nhiên route đích khác
        int dest = rand() % K;
        if (dest == src || routes[dest].load + customers[cust].demand > Capacity) continue;

        // Chọn ngẫu nhiên vị trí chèn trong route đích
        int insertPos = rand() % (routes[dest].nodes.size() + 1);

        // Tạo route mới để thử
        vector<int> newDestNodes = routes[dest].nodes;
        newDestNodes.insert(newDestNodes.begin() + insertPos, cust);
        double newDestLen = calculateRouteLength(newDestNodes);

        vector<int> newSrcNodes = routes[src].nodes;
        newSrcNodes.erase(newSrcNodes.begin() + posInSrc);
        double newSrcLen = calculateRouteLength(newSrcNodes);

        double newGlobalMax = max(newDestLen, newSrcLen);
        for (int v = 0; v < K; v++) {
            if (v != src && v != dest) {
                newGlobalMax = max(newGlobalMax, routes[v].length);
            }
        }

        if (newGlobalMax < currentMax - 1e-6) {
            // Chấp nhận move
            routes[dest].nodes = newDestNodes;
            routes[dest].length = newDestLen;
            routes[dest].load += customers[cust].demand;

            routes[src].nodes = newSrcNodes;
            routes[src].length = newSrcLen;
            routes[src].load -= customers[cust].demand;

            return true;
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
    const int NUM_ATTEMPTS = 800;  // Có thể giảm còn 500 nếu chậm

    for (int attempt = 0; attempt < NUM_ATTEMPTS; attempt++) {
        // Chọn ngẫu nhiên 2 route khác nhau
        int r1 = rand() % K;
        int r2 = rand() % K;
        if (r1 == r2 || routes[r1].nodes.empty() || routes[r2].nodes.empty()) continue;

        int n1 = routes[r1].nodes.size();
        int n2 = routes[r2].nodes.size();

        // Chọn ngẫu nhiên đoạn trên mỗi route (độ dài đoạn từ 1 đến min(3, size))
        int len1 = 1 + rand() % min(3, n1);
        int start1 = rand() % (n1 - len1 + 1);

        int len2 = 1 + rand() % min(3, n2);
        int start2 = rand() % (n2 - len2 + 1);

        // Tính load của 2 đoạn
        int load1 = 0, load2 = 0;
        for (int i = start1; i < start1 + len1; i++) load1 += customers[routes[r1].nodes[i]].demand;
        for (int i = start2; i < start2 + len2; i++) load2 += customers[routes[r2].nodes[i]].demand;

        int newLoad1 = routes[r1].load - load1 + load2;
        int newLoad2 = routes[r2].load - load2 + load1;
        if (newLoad1 > Capacity || newLoad2 > Capacity) continue;

        // Xây dựng route mới
        vector<int> newNodes1, newNodes2;

        // Route 1: trước start1 + đoạn từ route2 + sau start1+len1
        for (int i = 0; i < start1; i++) newNodes1.push_back(routes[r1].nodes[i]);
        for (int i = start2; i < start2 + len2; i++) newNodes1.push_back(routes[r2].nodes[i]);
        for (int i = start1 + len1; i < n1; i++) newNodes1.push_back(routes[r1].nodes[i]);

        // Route 2: tương tự
        for (int i = 0; i < start2; i++) newNodes2.push_back(routes[r2].nodes[i]);
        for (int i = start1; i < start1 + len1; i++) newNodes2.push_back(routes[r1].nodes[i]);
        for (int i = start2 + len2; i < n2; i++) newNodes2.push_back(routes[r2].nodes[i]);

        double newGlobalMax = max(calculateRouteLength(newNodes1), calculateRouteLength(newNodes2));
        for (int v = 0; v < K; v++) {
            if (v != r1 && v != r2) newGlobalMax = max(newGlobalMax, routes[v].length);
        }

        if (newGlobalMax < currentMax - 1e-6) {
            routes[r1].nodes = newNodes1;
            routes[r1].length = len1;
            routes[r1].load = newLoad1;

            routes[r2].nodes = newNodes2;
            routes[r2].length = len2;
            routes[r2].load = newLoad2;

            return true;
        }
    }
    return false;
}
// --- THUẬT TOÁN CHÍNH ---

int main() {
	freopen("input.txt", "r", stdin);
    // Nhập dữ liệu
    cin >> N >> K;
	Capacity = 100000000;
    customers.resize(N+10);
    N++;
    
    // Khởi tạo ma trận khoảng cách
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            cin>>distMatrix[i][j];
        }
    }
    
    for (int i = 0; i < N; i++) {
        customers[i].demand = 1;
    }


    // --- KHỞI TẠO (GREEDY) ---
	vector<Route> currentRoutes(K);
	vector<bool> visited(N, false);
	visited[0] = true;
	
	// Tạo danh sách các khách hàng (từ 1 đến N-1)
	vector<int> customerList;
	for (int i = 1; i < N; i++) {
	    customerList.push_back(i);
	}
	
	random_shuffle(customerList.begin(), customerList.end());
	
	for (int cust : customerList) {
	    if (visited[cust]) continue;
	
	    int bestVeh = -1;
	    double bestNewMax = 1e18;  // max length sau khi thêm khách này
	
	    for (int v = 0; v < K; v++) {
	        if (currentRoutes[v].load + customers[cust].demand > Capacity) continue;
	
	        // Thử tất cả vị trí chèn vào route v (để tìm vị trí tốt nhất)
	        double minAddedForThisVeh = 1e18;
	        double bestLenForThisVeh = 0;
	
	        for (int pos = 0; pos <= (int)currentRoutes[v].nodes.size(); pos++) {
	            vector<int> tempNodes = currentRoutes[v].nodes;
	            tempNodes.insert(tempNodes.begin() + pos, cust);
	
	            double newLen = calculateRouteLength(tempNodes);
	            if (newLen < minAddedForThisVeh) {
	                minAddedForThisVeh = newLen;
	            }
	        }
	        bestLenForThisVeh = minAddedForThisVeh;
	
	        // Tính max length của toàn bộ hệ thống nếu thêm vào xe v
	        double thisNewMax = bestLenForThisVeh;
	        for (int u = 0; u < K; u++) {
	            if (u != v) {
	                thisNewMax = max(thisNewMax, currentRoutes[u].length);
	            }
	        }
	
	        // Chọn xe làm tăng max length ít nhất
	        if (thisNewMax < bestNewMax - 1e-6 ||
	            (abs(thisNewMax - bestNewMax) < 1e-6 && currentRoutes[v].nodes.size() < currentRoutes[bestVeh].nodes.size())) {
	            bestNewMax = thisNewMax;
	            bestVeh = v;
	            // Lưu tạm bestLen để dùng sau
	            currentRoutes[v].length = bestLenForThisVeh; // tạm thời, sẽ cập nhật chính xác sau
	        }
	    }
	
	    // Sau khi tìm được xe tốt nhất, chèn khách vào vị trí tối ưu thực sự
	    if (bestVeh != -1) {
	        double minLen = 1e18;
	        int bestPos = -1;
	        for (int pos = 0; pos <= (int)currentRoutes[bestVeh].nodes.size(); pos++) {
	            vector<int> tempNodes = currentRoutes[bestVeh].nodes;
	            tempNodes.insert(tempNodes.begin() + pos, cust);
	            double len = calculateRouteLength(tempNodes);
	            if (len < minLen - 1e-6) {
	                minLen = len;
	                bestPos = pos;
	            }
	        }
	
	        // Chèn vào vị trí tốt nhất
	        currentRoutes[bestVeh].nodes.insert(currentRoutes[bestVeh].nodes.begin() + bestPos, cust);
	        currentRoutes[bestVeh].length = minLen;
	        currentRoutes[bestVeh].load += customers[cust].demand;
	        visited[cust] = true;
	    }
	}

    for (int i = 0; i < K; i++) currentRoutes[i].length = calculateRouteLength(currentRoutes[i].nodes);

    // --- RECORD-TO-RECORD (RTR) ---
    vector<Route> bestSolution = currentRoutes;
    double record = get_max_length(bestSolution);
    double deviation = 0.05; // Ngưỡng chấp nhận giải pháp tệ hơn một chút để thoát tối ưu cục bộ
	
	srand(time(NULL));
    auto startTime = chrono::steady_clock::now();
    
    int time = 2500;
    while (chrono::duration_cast<chrono::milliseconds>(chrono::steady_clock::now() - startTime).count() < time) {
        bool improved = false;
		cout<<record<<endl;
		
        // Áp dụng Local Search nội bộ mỗi route
        for (int i = 0; i < K; i++) {
            if (twoOpt(currentRoutes[i])) improved = true;
            if (orOptMove(currentRoutes[i])) improved = true;
        }

        // Inter-route - gọi nhiều lần để tăng cơ hội tìm move tốt
	    for (int t = 0; t < 5; t++) {  
	        if (relocate(currentRoutes)) improved = true;
	    }
	    for (int t = 0; t < 3; t++) { 
	        if (crossExchangeMove(currentRoutes)) improved = true;
	    }
			
        double currentMax = get_max_length(currentRoutes);
        
        // Cập nhật Record
        if (currentMax < record - 1e-6) {
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
    cout<<K<<endl;
    for (int i = 0; i < K; i++) {
        //cout << "Xe " << i + 1 << " (Load: " << bestSolution[i].load << "): 0 -> ";
        cout<<bestSolution[i].load+1<<endl;
        cout<<0<<" ";
        for (int node : bestSolution[i].nodes) cout << node << " ";
        cout<<endl;
        //cout << "0 | Length: " << bestSolution[i].length << endl;
    }

    return 0;
}
