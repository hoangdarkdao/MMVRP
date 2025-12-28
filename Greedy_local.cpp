#include <bits/stdc++.h>
using namespace std;

using ll = long long;
using db = double;

const db inf = 1e18;
const ll linf = 0x3f3f3f3f3f3f3f3f;

mt19937_64 rng(chrono::steady_clock::now().time_since_epoch().count());

int rnd(int l, int r) { return uniform_int_distribution<int>(l, r)(rng); }
db rnd_db(db l, db r) { return uniform_real_distribution<db>(l, r)(rng); }

template<class U, class V>
bool minimize(U &a, V b) { return b < a ? a = b, 1 : 0; }
template<class U, class V>
bool maximize(U &a, V b) { return a < b ? a = b, 1 : 0; }

const int N = 1e3 + 5;

int n, k, cap, id[N], dem[N];
db dist[N][N];
vector<db> X(N), Y(N);

struct Node {
  int i = 0;
  Node *L = NULL, *R = NULL;
  Node(int i) : i(i) {}
  Node(Node *L = NULL, Node *R = NULL) : L(L), R(R) {}
};

struct Route {
  int cnt = 1;
  db weight = 0;
  int used = 0; // used capacity
  Node *st = NULL, *en = NULL;
  Route() {
    st = new Node(0); // depot 0
    st->R = en = new Node(0); // end depot 0
    en->L = st;
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
  db obj, sum = 0;
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
  shuffle(id + 1, id + n, rng);
  for (int it = 1; it < n; it++) {
    int i = id[it];
    db mn = inf;
    int l = -1;
    Node *best = NULL;
    for (int j = 0; j < k; j++) {
      auto &r = sol.rt[j];
      if (r->used + dem[i] > cap) continue;
      auto itt = r->st;
      while (itt != r->en) {
        db delta = -dist[itt->i][itt->R->i] + dist[itt->i][i] + dist[i][itt->R->i];
        if (minimize(mn, r->weight + delta)) {
          l = j;
          best = itt;
        }
        itt = itt->R;
      }
    }
    if (~l) {
      auto &nod = sol.node[i];
      nod->R = best->R;
      nod->L = best;
      best->R->L = nod;
      best->R = nod;
      auto &r = sol.rt[l];
      r->weight += mn - r->weight; // update to new weight
      r->cnt++;
      r->used += dem[i];
    }
  }
  sol.init_st();
  return move(sol);
}

struct Obj {
  db mx, sum = 0;
  bool operator < (Obj &o) {
    if (mx != o.mx) return mx < o.mx;
    return sum < o.sum;
  }
};

struct Solution {
  Obj O;
  vector<db> w;
  vector<int> dsum;
  vector<vector<int>> rt;
  set<pair<db, int>> st;
  Solution() {}
  Solution(Sol &o, db ub) {
    int kk = o.rt.size();
    O.sum = o.sum;
    O.mx = o.obj;
    rt.resize(kk);
    w.resize(kk);
    dsum.resize(kk);
    for (int i = 0; i < kk; i++) {
      auto &r = o.rt[i];
      w[i] = r->weight;
      dsum[i] = r->used;
      st.insert({r->weight, i});
      auto it = r->st;
      while (it != r->en) {
        rt[i].push_back(it->i);
        it = it->R;
      }
      rt[i].push_back(0); // end depot 0
    }
  }

  bool operator < (Solution &o) {
    return O < o.O;
  }

  friend ostream& operator << (ostream &os, Solution &o) {
    os << o.rt.size() << "\n";
    for (int i = 0; i < o.rt.size(); i++) {
      os << o.rt[i].size() - 2 << "\n"; // number of customers
      for (int j = 1; j + 1 < o.rt[i].size(); j++) { // customers only
        os << o.rt[i][j] << " ";
      }
      os << "\n";
    }
    return os;
  }

  inline void rm(int x) { O.sum -= w[x]; st.erase({w[x], x}); }
  inline void add(int x) { O.sum += w[x]; st.insert({w[x], x}); }

  inline db mx2(int a, int b = -1) {
    for (auto it = st.rbegin(); it != st.rend(); ++it) {
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

  inline db seg(const vector<int> &r, int l, int rr) const {
    db s = 0;
    for (int i = l; i < rr; i++) s += dist[r[i]][r[i + 1]];
    return s;
  }

  Obj _oneMove(int x, int y, int u, int v) {
    if (!okv(x, y) || !oki(u, v)) return {inf, 0};
    if (x == u && (v == y || v == y + 1)) return {inf, 0};

    int moved_dem = dem[rt[x][y]];
    int new_dx = dsum[x] - moved_dem;
    int new_du = dsum[u] + moved_dem;
    if (new_du > cap) return {inf, 0};

    db mx = -inf;
    db sum = O.sum;
    if (x == u) {
      db &t = w[x];
      auto &r = rt[x];
      db _t = t - dist[r[y - 1]][r[y]] - dist[r[y]][r[y + 1]] + dist[r[y - 1]][r[y + 1]] - dist[r[v - 1]][r[v]] + dist[r[v - 1]][r[y]] + dist[r[y]][r[v]];
      maximize(mx, _t);
      sum += _t - t;
      maximize(mx, mx2(x));
      return {mx, sum};
    }

    db &z = w[x], &t = w[u];
    auto &a = rt[x];
    auto &b = rt[u];
    db _z = z - dist[a[y - 1]][a[y]] - dist[a[y]][a[y + 1]] + dist[a[y - 1]][a[y + 1]];
    db _t = t - dist[b[v - 1]][b[v]] + dist[b[v - 1]][a[y]] + dist[a[y]][b[v]];
    maximize(mx, _z);
    maximize(mx, _t);
    sum += _t + _z - t - z;
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj oneMove(int x, int y, int u, int v) {
    Obj cand = _oneMove(x, y, u, v);
    if (!(cand < O)) return O;

    int moved_dem = dem[rt[x][y]];
    dsum[x] -= moved_dem;
    dsum[u] += moved_dem;

    if (x == u) {
      rm(x);
      auto &r = rt[x];
      db t = w[x];
      w[x] = t - dist[r[y - 1]][r[y]] - dist[r[y]][r[y + 1]] + dist[r[y - 1]][r[y + 1]] - dist[r[v - 1]][r[v]] + dist[r[v - 1]][r[y]] + dist[r[y]][r[v]];
      if (y < v) {
        r.insert(r.begin() + v, r[y]);
        r.erase(r.begin() + y);
      } else {
        r.insert(r.begin() + v, r[y]);
        r.erase(r.begin() + y + 1);
      }
      add(x);
      O.mx = st.rbegin()->first;
      return O;
    }

    rm(x); rm(u);
    auto &a = rt[x];
    auto &b = rt[u];
    db z = w[x], t = w[u];
    w[x] = z - dist[a[y - 1]][a[y]] - dist[a[y]][a[y + 1]] + dist[a[y - 1]][a[y + 1]];
    w[u] = t - dist[b[v - 1]][b[v]] + dist[b[v - 1]][a[y]] + dist[a[y]][b[v]];
    b.insert(b.begin() + v, a[y]);
    a.erase(a.begin() + y);
    add(x); add(u);
    O.mx = st.rbegin()->first;
    return O;
  }

  Obj _swp2(int x, int y, int u, int v) {
    if (!okv(x, y) || !okv(u, v)) return {inf, 0};

    int demA = dem[rt[x][y]];
    int demB = dem[rt[u][v]];
    int new_dx = dsum[x] - demA + demB;
    int new_du = dsum[u] - demB + demA;
    if (new_dx > cap || new_du > cap) return {inf, 0};

    db mx = -inf;
    db sum = O.sum;

    if (x == u) {
      if (y == v) return {inf, 0};
      auto &r = rt[x];
      db t = w[x];
      int i = y, j = v;
      if (i > j) swap(i, j);
      int A = r[i], B = r[j];
      db nt = t;
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
    db za = w[x], tb = w[u];
    int A = a[y], B = b[v];
    db nza = za - dist[a[y - 1]][A] - dist[A][a[y + 1]] + dist[a[y - 1]][B] + dist[B][a[y + 1]];
    db ntb = tb - dist[b[v - 1]][B] - dist[B][b[v + 1]] + dist[b[v - 1]][A] + dist[A][b[v + 1]];
    maximize(mx, nza);
    maximize(mx, ntb);
    sum += (nza - za) + (ntb - tb);
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj swp2(int x, int y, int u, int v) {
    Obj cand = _swp2(x, y, u, v);
    if (!(cand < O)) return O;

    int demA = dem[rt[x][y]];
    int demB = dem[rt[u][v]];
    dsum[x] += demB - demA;
    dsum[u] += demA - demB;

    if (x == u) {
      rm(x);
      auto &r = rt[x];
      int i = y, j = v;
      if (i > j) swap(i, j);
      int A = r[i], B = r[j];
      db nt = w[x];
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
      O.mx = st.rbegin()->first;
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
    O.mx = st.rbegin()->first;
    return O;
  }

  Obj _opt2(int x, int i, int j) {
    if (!okv(x, i) || !okv(x, j)) return {inf, 0};
    if (i == j) return {inf, 0};
    if (i > j) swap(i, j);

    // no capacity change, same route
    auto &r = rt[x];
    db t = w[x];
    db nt = t - dist[r[i - 1]][r[i]] - dist[r[j]][r[j + 1]] + dist[r[i - 1]][r[j]] + dist[r[i]][r[j + 1]];
    db sum = O.sum + (nt - t);
    db mx = max(nt, mx2(x));
    return {mx, sum};
  }

  Obj opt2(int x, int i, int j) {
    Obj cand = _opt2(x, i, j);
    if (!(cand < O)) return O;

    rm(x);
    auto &r = rt[x];
    db t = w[x];
    w[x] = t - dist[r[i - 1]][r[i]] - dist[r[j]][r[j + 1]] + dist[r[i - 1]][r[j]] + dist[r[i]][r[j + 1]];
    reverse(r.begin() + i, r.begin() + j + 1);
    add(x);
    O.mx = st.rbegin()->first;
    return O;
  }

  Obj _opt2s(int x, int cutA, int u, int cutB) {
    if (x == u) return {inf, 0};
    if (!(0 <= x && x < rt.size() && 0 <= u && u < rt.size())) return {inf, 0};

    auto &A = rt[x];
    auto &B = rt[u];

    if (!(0 <= cutA && cutA + 1 < A.size())) return {inf, 0};
    if (!(0 <= cutB && cutB + 1 < B.size())) return {inf, 0};

    int dem_prefA = 0, dem_sufA = 0;
    for (int ii = 1; ii <= cutA; ++ii) dem_prefA += dem[A[ii]];
    for (int ii = cutA + 1; ii + 1 < A.size(); ++ii) dem_sufA += dem[A[ii]];

    int dem_prefB = 0, dem_sufB = 0;
    for (int ii = 1; ii <= cutB; ++ii) dem_prefB += dem[B[ii]];
    for (int ii = cutB + 1; ii + 1 < B.size(); ++ii) dem_sufB += dem[B[ii]];

    int new_dA = dem_prefA + dem_sufB;
    int new_dB = dem_prefB + dem_sufA;
    if (new_dA > cap || new_dB > cap) return {inf, 0};

    db wa = w[x], wb = w[u];

    db prefA = seg(A, 0, cutA);
    db sufA = seg(A, cutA + 1, (int)A.size() - 1);

    db prefB = seg(B, 0, cutB);
    db sufB = seg(B, cutB + 1, (int)B.size() - 1);

    db nA = prefA + dist[A[cutA]][B[cutB + 1]] + sufB;
    db nB = prefB + dist[B[cutB]][A[cutA + 1]] + sufA;

    db sum = O.sum + (nA - wa) + (nB - wb);
    db mx = max({nA, nB, mx2(x, u)});
    return {mx, sum};
  }

  Obj opt2s(int x, int cutA, int u, int cutB) {
    Obj cand = _opt2s(x, cutA, u, cutB);
    if (!(cand < O)) return O;

    auto &A = rt[x];
    auto &B = rt[u];

    dsum[x] = 0;
    for (int ii = 1; ii + 1 < A.size(); ++ii) dsum[x] += dem[A[ii]];
    dsum[u] = 0;
    for (int ii = 1; ii + 1 < B.size(); ++ii) dsum[u] += dem[B[ii]];

    rm(x); rm(u);

    db prefA = seg(A, 0, cutA);
    db sufA = seg(A, cutA + 1, (int)A.size() - 1);

    db prefB = seg(B, 0, cutB);
    db sufB = seg(B, cutB + 1, (int)B.size() - 1);

    db nA = prefA + dist[A[cutA]][B[cutB + 1]] + sufB;
    db nB = prefB + dist[B[cutB]][A[cutA + 1]] + sufA;

    vector<int> nR(A.begin(), A.begin() + cutA + 1);
    nR.insert(nR.end(), B.begin() + cutB + 1, B.end());

    vector<int> nS(B.begin(), B.begin() + cutB + 1);
    nS.insert(nS.end(), A.begin() + cutA + 1, A.end());

    A = nR;
    B = nS;

    w[x] = nA;
    w[u] = nB;

    add(x); add(u);
    O.mx = st.rbegin()->first;

    dsum[x] = 0;
    for (int ii = 1; ii + 1 < A.size(); ++ii) dsum[x] += dem[A[ii]];
    dsum[u] = 0;
    for (int ii = 1; ii + 1 < B.size(); ++ii) dsum[u] += dem[B[ii]];

    return O;
  }

  Obj _or(int x, int y, int len, int u, int v) {
    if (!(2 <= len && len <= 4)) return {inf, 0};
    if (!okv(x, y) || !oki(u, v)) return {inf, 0};

    auto &A = rt[x];
    if (y + len - 1 > (int)A.size() - 2) return {inf, 0};

    int moved_dem = 0;
    for (int ii = y; ii < y + len; ++ii) moved_dem += dem[A[ii]];

    int new_dx = dsum[x] - moved_dem;
    int new_du = dsum[u] + moved_dem;
    if (new_du > cap) return {inf, 0};

    db mx = -inf;
    db sum = O.sum;

    if (x == u) {
      if (v >= y && v <= y + len) return {inf, 0};
      db t = w[x];
      int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
      int a = A[v - 1], b = A[v];
      db nt = t - dist[pre][fst] - dist[lst][post] + dist[pre][post] - dist[a][b] + dist[a][fst] + dist[lst][b];
      maximize(mx, nt);
      sum += nt - t;
      maximize(mx, mx2(x));
      return {mx, sum};
    }

    auto &B = rt[u];
    db za = w[x], tb = w[u];
    int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
    db inner = seg(A, y, y + len - 1);
    db nza = za - dist[pre][fst] - inner - dist[lst][post] + dist[pre][post];
    int a = B[v - 1], b = B[v];
    db ntb = tb - dist[a][b] + dist[a][fst] + inner + dist[lst][b];
    maximize(mx, nza);
    maximize(mx, ntb);
    sum += (nza - za) + (ntb - tb);
    maximize(mx, mx2(x, u));
    return {mx, sum};
  }

  Obj orOpt(int x, int y, int len, int u, int v) {
    Obj cand = _or(x, y, len, u, v);
    if (!(cand < O)) return O;

    auto &A = rt[x];

    int moved_dem = 0;
    for (int ii = y; ii < y + len; ++ii) moved_dem += dem[A[ii]];
    dsum[x] -= moved_dem;
    dsum[u] += moved_dem;

    if (x == u) {
      rm(x);
      db t = w[x];
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
      O.mx = st.rbegin()->first;
      return O;
    }

    rm(x); rm(u);
    auto &B = rt[u];
    db za = w[x], tb = w[u];
    int pre = A[y - 1], fst = A[y], lst = A[y + len - 1], post = A[y + len];
    db inner = seg(A, y, y + len - 1);
    w[x] = za - dist[pre][fst] - inner - dist[lst][post] + dist[pre][post];
    int a = B[v - 1], b = B[v];
    w[u] = tb - dist[a][b] + dist[a][fst] + inner + dist[lst][b];
    vector<int> s(A.begin() + y, A.begin() + y + len);
    A.erase(A.begin() + y, A.begin() + y + len);
    B.insert(B.begin() + v, s.begin(), s.end());
    add(x); add(u);
    O.mx = st.rbegin()->first;
    return O;
  }

  int px() {
    int kk = rt.size();
    if (kk == 1) return 0;
    if (rnd_db(0, 1) < 0.92) return st.rbegin()->second;
    return rnd(0, kk - 1);
  }

  int pu(int x) {
    int kk = rt.size();
    if (kk <= 1) return x;
    if (rnd_db(0, 1) < 0.85) {
      for (auto it = st.begin(); it != st.end(); ++it) {
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
        int x = px(), uu = pu(x);
        int coin = rnd(0, 99);

        if (coin < 25) {  // oneMove
          int y = py(x);
          if (y == -1) continue;
          int uuu = (rnd_db(0, 1) < 0.90 ? uu : x);
          int v = pv(uuu);
          if (v == -1) continue;
          if (uuu == x && (v == y || v == y + 1)) continue;
          Obj c = _oneMove(x, y, uuu, v);
          if (c < best.v) {
            best.tp = 0; best.p[0]=x; best.p[1]=y; best.p[2]=uuu; best.p[3]=v; best.v=c;
          }
          continue;
        }

        if (coin < 45) {  // orOpt
          int len = rnd(2, 4);
          auto &A = rt[x];
          if ((int)A.size() - 2 < len) continue;
          int y = rnd(1, (int)A.size() - 2 - len + 1);
          int uuu = (rnd_db(0, 1) < 0.10 ? x : uu);
          int v = pv(uuu);
          if (v == -1) continue;
          if (uuu == x && (v >= y && v <= y + len)) continue;
          Obj c = _or(x, y, len, uuu, v);
          if (c < best.v) {
            best.tp = 1; best.p[0]=x; best.p[1]=y; best.p[2]=len; best.p[3]=uuu; best.p[4]=v; best.v=c;
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
          int y = py(x), v = py(uu);
          if (y == -1 || v == -1) continue;
          Obj c = _swp2(x, y, uu, v);
          if (c < best.v) {
            best.tp = 3; best.p[0]=x; best.p[1]=y; best.p[2]=uu; best.p[3]=v; best.v=c;
          }
          continue;
        }

        if (coin < 99) {  // opt2s
          auto &A = rt[x];
          auto &B = rt[uu];
          int ma = A.size(), mb = B.size();
          if (ma < 3 || mb < 3) continue;
          int ca = rnd(0, ma - 2);
          int cb = rnd(0, mb - 2);
          Obj c = _opt2s(x, ca, uu, cb);
          if (c < best.v) {
            best.tp = 4; best.p[0]=x; best.p[1]=ca; best.p[2]=uu; best.p[3]=cb; best.v=c;
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
        O = best.v;
        last = clk::now();
      }
    }
  }
};

int main() {
  cin.tie(0)->sync_with_stdio(0);

  cin >> n >> k;
  cin >> cap;
  for (int i = 0; i < n; i++) {
    cin >> X[i] >> Y[i] >> dem[i];
  }
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      dist[i][j] = sqrt((X[i] - X[j]) * (X[i] - X[j]) + (Y[i] - Y[j]) * (Y[i] - Y[j]));
    }
  }
  iota(id + 1, id + n, 1);

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
  	cerr << best.O.mx << "\n";
    db rem = TL - el();
    if (rem <= 0) break;
    Sol s = greedy();
    Solution cur(s, inf);
    cur.ls(rem, min(SL, rem), TRIES);
    if (cur < best) best = cur;
  }

  cerr << best.O.mx << "\n";
  cout << best;

  return 0;
}