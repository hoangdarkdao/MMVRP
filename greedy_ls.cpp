#include <bits/stdc++.h>
using namespace std;

using ll = long long;
using db = double;

const int inf = 0x3f3f3f3f;
const ll linf = 0x3f3f3f3f3f3f3f3f;

mt19937_64 rng(chrono::steady_clock::now().time_since_epoch().count());

int rnd(int l, int r) { return uniform_int_distribution<int>(l, r)(rng); }
db rnd_db(db l, db r) { return uniform_real_distribution<db>(l, r)(rng); }

template<class U, class V>
bool minimize(U &a, V b) { return b < a ? a = b, 1 : 0; }
template<class U, class V>
bool maximize(U &a, V b) { return a < b ? a = b, 1 : 0; }

const int N = 1e3 + 5;

int n, k, dist[N][N], id[N];

struct Node {
  int i = 0;
  Node *L = NULL, *R = NULL;
  Node(int i) : i(i) {}
  Node(Node *L = NULL, Node *R = NULL) : L(L), R(R) {}
};

struct Route {
  int cnt = 1;
  int weight = 0;
  Node *st = NULL, *en = NULL;
  Route() {
    st = new Node();
    st->R = en = new Node(st);
  }
  Route(Node *st, Node *en) : st(st), en(en) {}

  friend ostream& operator << (ostream &os, Route &rt) {
    os << rt.cnt << "\n";
    auto it = rt.st;
    while (it != rt.en) {
      os << it->i << " ";
      it = it->R;
    }
    return os;
  }
};

struct Sol {
  int obj, sum = 0;
  vector<Route*> rt;
  vector<Node*> node;
  Sol() {}
  Sol(int n, int k) {
    rt.resize(k);
    for (int i = 0; i < k; i++) rt[i] = new Route();
    node.resize(n);
    for (int i = 1; i < n; i++) node[i] = new Node(i);
  }

  void init_st() {
    sum = 0;
    obj = -inf;
    for (int i = 0; i < k; i++) {
      sum += rt[i]->weight;
      maximize(obj, rt[i]->weight);
    }
  }

  bool operator < (Sol &o) {
    if (obj != o.obj) return obj < o.obj;
    return sum < o.sum;
  }

  friend ostream& operator << (ostream &os, Sol &o) {
    os << k << "\n";
    for (int i = 0; i < k; i++) os << *(o.rt[i]) << "\n";
    return os;
  }
};

Sol greedy() {
  Sol sol(n, k);
  for (int i = 0; i < k; i++) {
    sol.rt[i]->en->i = n;
  }
  shuffle(id + 1, id + n, rng);
  for (int it = 1; it < n; it++) {
    int i = id[it];
    int mn = inf, l = -1;
    Node *best;
    for (int j = 0; j < k; j++) {
      auto &r = sol.rt[j];
      auto it = r->st;
      while (it != r->en) {
        if (minimize(mn, r->weight - dist[it->i][it->R->i] + dist[it->i][i] + dist[i][it->R->i])) {
          l = j;
          best = it;
        }
        it = it->R;
      }
    }
    if (~l) {
      auto &n = sol.node[i];
      n->R = best->R;
      n->L = best;
      best->R->L = n;
      best->R = n;
      auto &r = sol.rt[l];
      r->weight = mn;
      r->cnt++;
    }
  }
  sol.init_st();
  return move(sol);
}

struct Obj {
  int mx, sum = 0;
  bool operator < (Obj &o) {
    if (mx != o.mx) return mx < o.mx;
    return sum < o.sum;
  }
};

struct Solution {
  Obj O;
  vector<int> w;
  vector<vector<int>> rt;
  set<pair<int, int>> st;
  Solution() {}
  Solution(Sol &o, int ub) {
    O.sum = o.sum;
    O.mx = o.obj;
    int k = o.rt.size();
    rt.resize(k);
    w.resize(k);
    for (int i = 0; i < k; i++) {
      auto &r = o.rt[i];
      w[i] = r->weight;
      st.insert({r->weight, i});
      auto it = r->st;
      while (it != r->en) {
        rt[i].push_back(it->i);
        it = it->R;
      }
      rt[i].push_back(n);
    }
  }

  bool operator < (Solution &o) {
    return O < o.O;
  }

  friend ostream& operator << (ostream &os, Solution &o) {
    os << k << "\n";
    for (int i = 0; i < k; i++) {
      os << o.rt[i].size() - 1 << "\n";
      for (int j = 0; j + 1 < o.rt[i].size(); j++) {
        os << o.rt[i][j] << " ";
      }
      os << "\n";
    }
    return os;
  }

  inline void rm(int x) { O.sum -= w[x]; st.erase({w[x], x}); }
  inline void add(int x) { O.sum += w[x]; st.insert({w[x], x}); }

  inline int mx2(int a, int b = -1) {
    for (auto it = rbegin(st); it != rend(st); it++) {
      int id = it->second;
      if (id != a && id != b) return it->first;
    }
    return -inf;
  }

  inline bool okv(int x, int y) const {
    return 0 <= x && x < rt.size() && 0 < y && y + 1 < rt[x].size();
  }
  inline bool oki(int x, int y) const {
    return 0 <= x && x < rt.size() && 0 < y && y < rt[x].size();
  }

  inline int seg(const vector<int> &r, int l, int rr) const {
    int s = 0;
    for (int i = l; i < rr; i++) s += dist[r[i]][r[i + 1]];
    return s;
  }

  Obj _oneMove(int x, int y, int u, int v) {
    if (!okv(x, y) || !oki(u, v)) return O;
    if (x == u && (v == y || v == y + 1)) return O;

    int mx = -inf;
    int sum = O.sum;
    if (x == u) {
      int &t = w[x];
      auto &r = rt[x];
      int _t = t - dist[r[y - 1]][r[y]] - dist[r[y]][r[y + 1]] + dist[r[y - 1]][r[y + 1]] - dist[r[v - 1]][r[v]] + dist[r[v - 1]][r[y]] + dist[r[y]][r[v]];
      maximize(mx, _t);
      sum += _t - t;
      maximize(mx, mx2(x));
      return {mx, sum};
    }

    int &z = w[x], &t = w[u];
    auto &a = rt[x];
    auto &b = rt[u];
    int _z = z - dist[a[y - 1]][a[y]] - dist[a[y]][a[y + 1]] + dist[a[y - 1]][a[y + 1]];
    int _t = t - dist[b[v - 1]][b[v]] + dist[b[v - 1]][a[y]] + dist[a[y]][b[v]];
    maximize(mx, _z);
    maximize(mx, _t);
    sum += _t + _z - t - z;
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj oneMove(int x, int y, int u, int v) {
    if (!okv(x, y) || !oki(u, v)) return O;
    if (x == u && (v == y || v == y + 1)) return O;

    if (x == u) {
      rm(x);
      auto &r = rt[x];
      int t = w[x];
      w[x] = t - dist[r[y - 1]][r[y]] - dist[r[y]][r[y + 1]] + dist[r[y - 1]][r[y + 1]] - dist[r[v - 1]][r[v]] + dist[r[v - 1]][r[y]] + dist[r[y]][r[v]];
      if (y < v) {
        r.insert(r.begin() + v, r[y]);
        r.erase(r.begin() + y);
      } else {
        r.insert(r.begin() + v, r[y]);
        r.erase(r.begin() + y + 1);
      }
      add(x);
      O.mx = rbegin(st)->first;
      return O;
    }

    rm(x); rm(u);
    auto &a = rt[x];
    auto &b = rt[u];
    int z = w[x], t = w[u];
    w[x] = z - dist[a[y - 1]][a[y]] - dist[a[y]][a[y + 1]] + dist[a[y - 1]][a[y + 1]];
    w[u] = t - dist[b[v - 1]][b[v]] + dist[b[v - 1]][a[y]] + dist[a[y]][b[v]];
    b.insert(b.begin() + v, a[y]);
    a.erase(a.begin() + y);
    add(x); add(u);
    O.mx = rbegin(st)->first;
    return O;
  }

  Obj _swp2(int x, int y, int u, int v) {
    if (!okv(x, y) || !okv(u, v)) return O;

    int mx = -inf;
    int sum = O.sum;

    if (x == u) {
      if (y == v) return O;
      auto &r = rt[x];
      int t = w[x];
      int i = y, j = v;
      if (i > j) swap(i, j);
      int A = r[i], B = r[j];
      int nt = t;
      if (j == i + 1) {
        int pre = r[i - 1], post = r[j + 1];
        nt = t - dist[pre][A] - dist[A][B] - dist[B][post] + dist[pre][B] + dist[B][A] + dist[A][post];
      } else {
        int pi = r[i - 1], ni = r[i + 1];
        int pj = r[j - 1], nj = r[j + 1];
        nt = t - dist[pi][A] - dist[A][ni] - dist[pj][B] - dist[B][nj] + dist[pi][B] + dist[B][ni] + dist[pj][A] + dist[A][nj];
      }
      maximize(mx, nt);
      sum += nt - t;
      maximize(mx, mx2(x));
      return {mx, sum};
    }

    auto &a = rt[x];
    auto &b = rt[u];
    int za = w[x], tb = w[u];
    int A = a[y], B = b[v];
    int nza = za - dist[a[y - 1]][A] - dist[A][a[y + 1]] + dist[a[y - 1]][B] + dist[B][a[y + 1]];
    int ntb = tb - dist[b[v - 1]][B] - dist[B][b[v + 1]] + dist[b[v - 1]][A] + dist[A][b[v + 1]];
    maximize(mx, nza);
    maximize(mx, ntb);
    sum += (nza - za) + (ntb - tb);
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj swp2(int x, int y, int u, int v) {
    if (!okv(x, y) || !okv(u, v)) return O;

    if (x == u) {
      if (y == v) return O;
      rm(x);
      auto &r = rt[x];
      int i = y, j = v;
      if (i > j) swap(i, j);
      int A = r[i], B = r[j];
      int nt = w[x];
      if (j == i + 1) {
        int pre = r[i - 1], post = r[j + 1];
        nt = w[x] - dist[pre][A] - dist[A][B] - dist[B][post] + dist[pre][B] + dist[B][A] + dist[A][post];
      } else {
        int pi = r[i - 1], ni = r[i + 1];
        int pj = r[j - 1], nj = r[j + 1];
        nt = w[x] - dist[pi][A] - dist[A][ni] - dist[pj][B] - dist[B][nj] + dist[pi][B] + dist[B][ni] + dist[pj][A] + dist[A][nj];
      }
      w[x] = nt;
      swap(r[i], r[j]);
      add(x);
      O.mx = rbegin(st)->first;
      return O;
    }

    rm(x); rm(u);
    auto &a = rt[x];
    auto &b = rt[u];
    int A = a[y], B = b[v];
    w[x] = w[x] - dist[a[y - 1]][A] - dist[A][a[y + 1]] + dist[a[y - 1]][B] + dist[B][a[y + 1]];
    w[u] = w[u] - dist[b[v - 1]][B] - dist[B][b[v + 1]] + dist[b[v - 1]][A] + dist[A][b[v + 1]];
    swap(a[y], b[v]);
    add(x); add(u);
    O.mx = rbegin(st)->first;
    return O;
  }

  Obj _opt2(int x, int i, int j) {
    if (!okv(x, i) || !okv(x, j)) return O;
    if (i == j) return O;
    if (i > j) swap(i, j);

    auto &r = rt[x];
    int t = w[x];
    int nt = t - dist[r[i - 1]][r[i]] - dist[r[j]][r[j + 1]] + dist[r[i - 1]][r[j]] + dist[r[i]][r[j + 1]];
    int sum = O.sum + (nt - t);
    int mx = max(nt, mx2(x));
    return {mx, sum};
  }

  Obj opt2(int x, int i, int j) {
    if (!okv(x, i) || !okv(x, j)) return O;
    if (i == j) return O;
    if (i > j) swap(i, j);

    rm(x);
    auto &r = rt[x];
    int t = w[x];
    w[x] = t - dist[r[i - 1]][r[i]] - dist[r[j]][r[j + 1]] + dist[r[i - 1]][r[j]] + dist[r[i]][r[j + 1]];
    reverse(r.begin() + i, r.begin() + j + 1);
    add(x);
    O.mx = rbegin(st)->first;
    return O;
  }

  Obj _opt2s(int x, int cutA, int u, int cutB) {
    if (x == u) return O;
    if (!(0 <= x && x < rt.size() && 0 <= u && u < rt.size())) return O;

    auto &A = rt[x];
    auto &B = rt[u];

    if (!(0 <= cutA && cutA + 1 < A.size())) return O;
    if (!(0 <= cutB && cutB + 1 < B.size())) return O;

    int wa = w[x], wb = w[u];

    int prefA = seg(A, 0, cutA);
    int sufA = seg(A, cutA + 1, (int)A.size() - 1);

    int prefB = seg(B, 0, cutB);
    int sufB = seg(B, cutB + 1, (int)B.size() - 1);

    int nA = prefA + dist[A[cutA]][B[cutB + 1]] + sufB;
    int nB = prefB + dist[B[cutB]][A[cutA + 1]] + sufA;

    int sum = O.sum + (nA - wa) + (nB - wb);
    int mx = max({nA, nB, mx2(x, u)});
    return {mx, sum};
  }

  Obj opt2s(int x, int cutA, int u, int cutB) {
    if (x == u) return O;
    if (!(0 <= x && x < rt.size() && 0 <= u && u < rt.size())) return O;

    auto &A = rt[x];
    auto &B = rt[u];

    if (!(0 <= cutA && cutA + 1 < A.size())) return O;
    if (!(0 <= cutB && cutB + 1 < B.size())) return O;

    rm(x); rm(u);

    int prefA = seg(A, 0, cutA);
    int sufA = seg(A, cutA + 1, (int)A.size() - 1);

    int prefB = seg(B, 0, cutB);
    int sufB = seg(B, cutB + 1, (int)B.size() - 1);

    int nA = prefA + dist[A[cutA]][B[cutB + 1]] + sufB;
    int nB = prefB + dist[B[cutB]][A[cutA + 1]] + sufA;

    vector<int> nR(A.begin(), A.begin() + cutA + 1);
    nR.insert(nR.end(), B.begin() + cutB + 1, B.end());

    vector<int> nS(B.begin(), B.begin() + cutB + 1);
    nS.insert(nS.end(), A.begin() + cutA + 1, A.end());

    A = nR;
    B = nS;

    w[x] = nA;
    w[u] = nB;

    add(x); add(u);
    O.mx = rbegin(st)->first;
    return O;
  }

  Obj _or(int x, int y, int len, int u, int v) {
    if (!(2 <= len && len <= 4)) return O;
    if (!okv(x, y) || !oki(u, v)) return O;

    auto &A = rt[x];
    if (y + len - 1 > (int)A.size() - 2) return O;

    int mx = -inf;
    int sum = O.sum;

    if (x == u) {
      if (v >= y && v <= y + len) return O;
      int t = w[x];
      int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
      int a = A[v - 1], b = A[v];
      int nt = t - dist[pre][fst] - dist[lst][post] + dist[pre][post] - dist[a][b] + dist[a][fst] + dist[lst][b];
      maximize(mx, nt);
      sum += nt - t;
      maximize(mx, mx2(x));
      return {mx, sum};
    }

    auto &B = rt[u];
    int za = w[x], tb = w[u];
    int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
    int inner = seg(A, y, y + len - 1);
    int nza = za - dist[pre][fst] - inner - dist[lst][post] + dist[pre][post];
    int a = B[v - 1], b = B[v];
    int ntb = tb - dist[a][b] + dist[a][fst] + inner + dist[lst][b];
    maximize(mx, nza);
    maximize(mx, ntb);
    sum += (nza - za) + (ntb - tb);
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj orOpt(int x, int y, int len, int u, int v) {
    if (!(2 <= len && len <= 4)) return O;
    if (!okv(x, y) || !oki(u, v)) return O;

    auto &A = rt[x];
    if (y + len - 1 > (int)A.size() - 2) return O;

    if (x == u) {
      if (v >= y && v <= y + len) return O;
      rm(x);
      int t = w[x];
      int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
      int a = A[v - 1], b = A[v];
      w[x] = t - dist[pre][fst] - dist[lst][post] + dist[pre][post] - dist[a][b] + dist[a][fst] + dist[lst][b];
      vector<int> s(A.begin() + y, A.begin() + y + len);
      if (y < v) {
        A.insert(A.begin() + v, s.begin(), s.end());
        A.erase(A.begin() + y, A.begin() + y + len);
      } else {
        A.insert(A.begin() + v, s.begin(), s.end());
        A.erase(A.begin() + y + len, A.begin() + y + 2 * len);
      }
      add(x);
      O.mx = rbegin(st)->first;
      return O;
    }

    rm(x); rm(u);
    auto &B = rt[u];
    int za = w[x], tb = w[u];
    int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
    int inner = seg(A, y, y + len - 1);
    w[x] = za - dist[pre][fst] - inner - dist[lst][post] + dist[pre][post];
    int a = B[v - 1], b = B[v];
    w[u] = tb - dist[a][b] + dist[a][fst] + inner + dist[lst][b];
    vector<int> s(A.begin() + y, A.begin() + y + len);
    A.erase(A.begin() + y, A.begin() + y + len);
    B.insert(B.begin() + v, s.begin(), s.end());
    add(x); add(u);
    O.mx = rbegin(st)->first;
    return O;
  }

  int px() {
    int kk = rt.size();
    if (kk == 1) return 0;
    if (rnd_db(0, 1) < 0.92) return rbegin(st)->second;
    return rnd(0, kk - 1);
  }

  int pu(int x) {
    int kk = rt.size();
    if (kk <= 1) return x;
    if (rnd_db(0, 1) < 0.85) {
      for (auto it = begin(st); it != end(st); it++) {
        if (it->second != x) return it->second;
      }
    }
    int u = x;
    while (u == x) u = rnd(0, kk - 1);
    return u;
  }

  int py(int x) {
    int m = rt[x].size();
    if (m < 3) return -1;
    if (m == 3) return 1;
    return rnd(1, m - 2);
  }

  int pv(int x) {
    int m = rt[x].size();
    if (m < 2) return -1;
    return rnd(1, m - 1);
  }

  void ls(db tl, db sl, int tries = 96) {
    using clk = chrono::steady_clock;
    auto t0 = clk::now();
    auto last = t0;

    auto el = [&]() { return chrono::duration<db>(clk::now() - t0).count(); };
    auto noimp = [&]() { return chrono::duration<db>(clk::now() - last).count(); };

    struct Mv { int tp, p[8]; Obj v; };

    while (el() < tl && noimp() < sl) {
      Mv best;
      best.tp = -1;
      best.v = O;

      for (int t = 0; t < tries && el() < tl; t++) {
        int x = px(), u = pu(x);
        int coin = rnd(0, 99);

        if (coin < 25) {  // oneMove
          int y = py(x);
          if (y == -1) continue;
          int uu = (rnd_db(0, 1) < 0.90 ? u : x);
          int v = pv(uu);
          if (v == -1) continue;
          if (uu == x && (v == y || v == y + 1)) continue;
          Obj c = _oneMove(x, y, uu, v);
          if (c < best.v) {
            best.tp = 0; best.p[0]=x; best.p[1]=y; best.p[2]=uu; best.p[3]=v; best.v=c;
          }
          continue;
        }

        if (coin < 45) {  // orOpt
          int len = rnd(2, 4);
          auto &A = rt[x];
          if ((int)A.size() - 2 < len) continue;
          int y = rnd(1, (int)A.size() - 2 - len + 1);
          int uu = (rnd_db(0, 1) < 0.10 ? x : u);
          int v = pv(uu);
          if (v == -1) continue;
          if (uu == x && (v >= y && v <= y + len)) continue;
          Obj c = _or(x, y, len, uu, v);
          if (c < best.v) {
            best.tp = 1; best.p[0]=x; best.p[1]=y; best.p[2]=len; best.p[3]=uu; best.p[4]=v; best.v=c;
          }
          continue;
        }

        if (coin < 65) {  // opt2
          auto &A = rt[x];
          int m = A.size();
          if (m < 5) continue;
          int i = rnd(1, m - 3);
          int j = rnd(i + 1, m - 2);
          Obj c = _opt2(x, i, j);
          if (c < best.v) {
            best.tp = 2; best.p[0]=x; best.p[1]=i; best.p[2]=j; best.v=c;
          }
          continue;
        }

        if (coin < 80) {  // swp2
          int y = py(x), v = py(u);
          if (y == -1 || v == -1) continue;
          Obj c = _swp2(x, y, u, v);
          if (c < best.v) {
            best.tp = 3; best.p[0]=x; best.p[1]=y; best.p[2]=u; best.p[3]=v; best.v=c;
          }
          continue;
        }

        if (coin < 99) {  // opt2s
          auto &A = rt[x];
          auto &B = rt[u];
          int ma = A.size(), mb = B.size();
          if (ma < 3 || mb < 3) continue;
          int ca = rnd(0, ma - 2);
          int cb = rnd(0, mb - 2);
          Obj c = _opt2s(x, ca, u, cb);
          if (c < best.v) {
            best.tp = 4; best.p[0]=x; best.p[1]=ca; best.p[2]=u; best.p[3]=cb; best.v=c;
          }
          continue;
        }
      }

      if (best.tp != -1 && best.v < O) {
        if (best.tp == 0) oneMove(best.p[0], best.p[1], best.p[2], best.p[3]);
        else if (best.tp == 1) orOpt(best.p[0], best.p[1], best.p[2], best.p[3], best.p[4]);
        else if (best.tp == 2) opt2(best.p[0], best.p[1], best.p[2]);
        else if (best.tp == 3) swp2(best.p[0], best.p[1], best.p[2], best.p[3]);
        else if (best.tp == 4) opt2s(best.p[0], best.p[1], best.p[2], best.p[3]);
        last = clk::now();
      }
    }
  }
};

int main() {
  //freopen("input.txt", "r", stdin);
  cin.tie(0)->sync_with_stdio(0);

  cin >> n >> k;
  n++;
  iota(id + 1, id + n, 1);
  int mn = inf;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      cin >> dist[i][j];
      if (i != j) minimize(mn, dist[i][j]);
    }
  }

  db TL = 29.5;
  db SL = 2;
  int TRIES = 200;

  using clk = chrono::steady_clock;
  auto t0 = clk::now();
  auto el = [&]() { return chrono::duration<db>(clk::now() - t0).count(); };

  Sol s0 = greedy();
  Solution best(s0, inf);
  best.ls(TL - el(), SL, TRIES);

  while (el() < TL) {
    db rem = TL - el();
    if (rem <= 0) break;
    Sol s = greedy();
    Solution cur(s, inf);
    cur.ls(rem, min(SL, rem), TRIES);
    if (cur < best) best = cur;
  }

  //cerr << best.O.mx << "\n";
  cout << best;

  return 0;
}