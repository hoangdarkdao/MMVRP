#include <bits/stdc++.h>
using namespace std;

int main() {
    int n, m;
    cin >> n >> m;
    double capacity;
    cin >> capacity;

    vector<double> x(n), y(n);
    vector<double> demand(n);
    for (int i = 0; i < n; ++i) {
        cin >> x[i] >> y[i] >> demand[i];
    }

    for (int i = 1; i < n; ++i) {
        if (demand[i] - capacity > 1e-12) {
            cout << "NO SOLUTION\n";
            return 0;
        }
    }

    vector<vector<double>> dist(n, vector<double>(n, 0.0));
    for (int i = 0; i < n; ++i) {
        for (int j = i+1; j < n; ++j) {
            double dx = x[i] - x[j];
            double dy = y[i] - y[j];
            double d = sqrt(dx*dx + dy*dy);
            dist[i][j] = dist[j][i] = d;
        }
    }

    vector<bool> served(n, false);
    served[0] = true; 
    int remaining_customers = n - 1;

    vector<vector<int>> routes;
    vector<double> route_lengths;

    double total_distance = 0.0;

    for (int veh = 0; veh < m; ++veh) {
        if (remaining_customers == 0) break;

        double rem_cap = capacity;
        int cur = 0; 
        vector<int> route;
        route.push_back(0);

        while (true) {
            int best = -1;
            double bestd = 1e300;
            for (int j = 1; j < n; ++j) {
                if (served[j]) continue;
                if (demand[j] - rem_cap > 1e-12) continue; 
                if (dist[cur][j] < bestd) {
                    bestd = dist[cur][j];
                    best = j;
                }
            }
            if (best == -1) break; 
            route.push_back(best);
            served[best] = true;
            rem_cap -= demand[best];
            --remaining_customers;
            cur = best;
            if (remaining_customers == 0) break;
        }

        route.push_back(0);
        double rlen = 0.0;
        for (size_t k = 0; k + 1 < route.size(); ++k) {
            rlen += dist[ route[k] ][ route[k+1] ];
        }
        routes.push_back(route);
        route_lengths.push_back(rlen);
        total_distance += rlen;
    }

    if (remaining_customers > 0) {
        cout << "NO SOLUTION\n";
        return 0;
    }

    cout << fixed << setprecision(2);
    cout << "Total distance: " << total_distance << "\n";
    for (size_t i = 0; i < routes.size(); ++i) {
        cout << "Vehicle " << i+1 << ": ";
        for (size_t j = 0; j < routes[i].size(); ++j) {
            cout << routes[i][j];
            if (j + 1 < routes[i].size()) cout << " -> ";
        }
        cout << " (route length: " << route_lengths[i] << ")\n";
    }

    return 0;
}
