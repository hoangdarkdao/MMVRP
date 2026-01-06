#include <bits/stdc++.h>
using namespace std;

struct Customer {
    double x, y;
    int demand;
};

const double INF = 1e18;
int n, k, CAP;
vector<Customer> a;

double dist(int i, int j) {
    return hypot(a[i].x - a[j].x, a[i].y - a[j].y);
}

/* ================= DEPOT-AWARE CAPACITY KMEANS ================= */

vector<vector<int>> balanced_kmeans(int ITER = 40) {
    vector<int> customers;
    for (int i = 1; i < n; i++) customers.push_back(i);

    vector<vector<int>> best;
    double bestMax = INF;

    const double alpha = 0.6;
    const double beta  = 0.0005;

    for (int it = 0; it < ITER; it++) {
        random_shuffle(customers.begin(), customers.end());

        vector<int> centroid;
        for (int i = 0; i < k; i++)
            centroid.push_back(customers[i]);

        vector<vector<int>> cluster(k);
        vector<int> load(k, 0);

        for (int c : customers) {
            double bestCost = INF;
            int bestId = -1;

            for (int i = 0; i < k; i++) {
                if (load[i] + a[c].demand > CAP) continue;

                double cost =
                    alpha * dist(c, centroid[i]) +
                    (1 - alpha) * dist(c, 0) +
                    beta * load[i];

                if (cost < bestCost) {
                    bestCost = cost;
                    bestId = i;
                }
            }

            if (bestId == -1) continue;
            cluster[bestId].push_back(c);
            load[bestId] += a[c].demand;
        }

        // update centroid
        for (int i = 0; i < k; i++) {
            if (cluster[i].empty()) continue;
            double sx = 0, sy = 0;
            for (int v : cluster[i]) {
                sx += a[v].x;
                sy += a[v].y;
            }
            sx /= cluster[i].size();
            sy /= cluster[i].size();

            int bestV = cluster[i][0];
            double bestD = INF;
            for (int v : cluster[i]) {
                double d = hypot(a[v].x - sx, a[v].y - sy);
                if (d < bestD) {
                    bestD = d;
                    bestV = v;
                }
            }
            centroid[i] = bestV;
        }

        // quick eval
        double curMax = 0;
        for (int i = 0; i < k; i++) {
            double r = 0;
            for (int v : cluster[i])
                r += dist(0, v);
            curMax = max(curMax, r);
        }

        if (curMax < bestMax) {
            bestMax = curMax;
            best = cluster;
        }
    }
    return best;
}

/* ================= INSERTION ================= */
vector<int> regret_insertion(const vector<int>& cluster) {
    vector<int> route = {0, 0};
    vector<bool> used(n, false);

    if (cluster.empty()) return route;

    // random start
    int start = cluster[rand() % cluster.size()];
    route.insert(route.begin() + 1, start);
    used[start] = true;

    while (true) {
        int chosen = -1;
        int bestPos = -1;
        double bestScore = -1e18;

        for (int v : cluster) {
            if (used[v]) continue;

            double best1 = INF, best2 = INF;
            int pos1 = -1;

            for (int i = 0; i + 1 < (int)route.size(); i++) {
                double inc = dist(route[i], v) +
                             dist(v, route[i + 1]) -
                             dist(route[i], route[i + 1]);

                if (inc < best1) {
                    best2 = best1;
                    best1 = inc;
                    pos1 = i + 1;
                } else if (inc < best2) {
                    best2 = inc;
                }
            }

            if (best2 == INF) continue;

            double regret = best2 - best1;
            double score = regret - 0.2 * best1; 

            if (score > bestScore) {
                bestScore = score;
                chosen = v;
                bestPos = pos1;
            }
        }

        if (chosen == -1) break;

        route.insert(route.begin() + bestPos, chosen);
        used[chosen] = true;
    }

    return route;
}

/* ================= MAIN ================= */

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    cin >> n >> k;
    cin >> CAP;

    a.resize(n);
    for (int i = 0; i < n; i++)
        cin >> a[i].x >> a[i].y >> a[i].demand;

    auto clusters = balanced_kmeans();

    double globalMax = 0;
    cout << fixed << setprecision(4);

    for (int i = 0; i < k; i++) {
        auto route = regret_insertion(clusters[i]);

        double len = 0;
        for (int j = 0; j + 1 < (int)route.size(); j++)
            len += dist(route[j], route[j + 1]);

        globalMax = max(globalMax, len);

        cout << "Route " << i + 1 << ": ";
        for (int v : route) cout << v << " ";
        cout << "| Length = " << len << "\n";
    }

    cout << "Min-Max Value = " << globalMax << "\n";
    return 0;
}
