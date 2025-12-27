#include <bits/stdc++.h>
using namespace std;

const int N = 505;

struct Node {
    int i;
    Node *L, *R;
    Node(int _i = 0) : i(_i), L(nullptr), R(nullptr) {}
};

struct Customer {
    double x, y;
    long long demand;
};

struct Route {
    double weight = 0;
    long long load = 0;
    Node *st, *en;

    Route() {
        st = new Node(0);
        en = new Node(0);
        st->R = en;
        en->L = st;
    }
};

int n, k;
long long CAP;
Customer cus[N];
double dist[N][N];
int id[N];
double Maxlen;

double calc_dist(int i, int j) {
    double dx = cus[i].x - cus[j].x;
    double dy = cus[i].y - cus[j].y;
    return sqrt(dx * dx + dy * dy);
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);
	
    cin >> n >> k;
    n--;
    cin >> CAP;

    for (int i = 0; i <= n; i++) {
        cin >> cus[i].x >> cus[i].y >> cus[i].demand;
    }

    for (int i = 0; i <= n; i++)
        for (int j = 0; j <= n; j++)
            dist[i][j] = calc_dist(i, j);

    iota(id + 1, id + n + 1, 1);
    mt19937 rng(chrono::steady_clock::now().time_since_epoch().count());
    shuffle(id + 1, id + n + 1, rng);

    vector<Route> routes(k);

    for (int t = 1; t <= n; t++) {
        int i = id[t];
        double best = 1e18;
        int best_r = -1;
        Node *best_pos = nullptr;

        for (int r = 0; r < k; r++) {
            if (routes[r].load + cus[i].demand > CAP) continue;

            for (Node *it = routes[r].st; it->R; it = it->R) {
                if (it->R == nullptr) break;
                double delta =
                    dist[it->i][i] +
                    dist[i][it->R->i] -
                    dist[it->i][it->R->i];

                if (delta < best) {
                    best = delta;
                    best_r = r;
                    best_pos = it;
                }
            }
        }

        if (best_r != -1) {
            Node *nd = new Node(i);
            nd->R = best_pos->R;
            nd->L = best_pos;
            best_pos->R->L = nd;
            best_pos->R = nd;

            routes[best_r].weight += best;
            routes[best_r].load += cus[i].demand;
        }
    }

    cout << k << "\n";
    for (int r = 0; r < k; r++) {
        Node *it = routes[r].st->R;
        cout << "Route " << r + 1 << ": ";
        while (it != routes[r].en) {
            cout << it->i << " ";
            it = it->R;
        }
        cout << "| load = " << routes[r].load
             << ", length = " << routes[r].weight << "\n";
        Maxlen = max(Maxlen, routes[r].weight);
    }
    cout<<Maxlen;
    
    return 0;
}
