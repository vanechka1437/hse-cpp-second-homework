#include <iostream>
#include <array>
#include <vector>
#include <random>
#include <algorithm>
#include <cstring>
#include <limits>
#include <cassert>
#include <regex>
#include <fstream>

namespace types {

    template <std::size_t N, std::size_t K, bool Fast = false>
    class Fixed {
    public:
        using ValueType = std::conditional_t<
                Fast,
                std::conditional_t<N <= 8, int_fast8_t,
                        std::conditional_t<N <= 16, int_fast16_t,
                                std::conditional_t<N <= 32, int_fast32_t, int_fast64_t>>>,
                std::conditional_t<N <= 8, int8_t,
                        std::conditional_t<N <= 16, int16_t,
                                std::conditional_t<N <= 32, int32_t, int64_t>>>>;

        static constexpr std::size_t TotalBits = N;
        static constexpr std::size_t FractionBits = K;

        ValueType value = 0;

    public:
        constexpr Fixed(int integer = 0) : value(static_cast<ValueType>(integer) << K) {}
        constexpr Fixed(double real) : value(static_cast<ValueType>(real * (1 << K))) {}

        template <std::size_t OtherN, std::size_t OtherK, bool OtherFast>
        constexpr Fixed(const Fixed<OtherN, OtherK, OtherFast>& other) {
            if constexpr (K > OtherK) {
                value = static_cast<ValueType>(other.rawValue() << (K - OtherK));
            } else {
                value = static_cast<ValueType>(other.rawValue() >> (OtherK - K));
            }
        }

        static constexpr Fixed fromRaw(ValueType raw) {
            Fixed result;
            result.value = raw;
            return result;
        }

        ValueType rawValue() const { return value; }

        explicit operator float() const { return static_cast<float>(value) / (1 << K); }
        explicit operator double() const { return static_cast<double>(value) / (1 << K); }

        auto operator<=>(const Fixed&) const = default;
        bool operator==(const Fixed&) const = default;

        // Arithmetic operators
        friend Fixed operator+(Fixed lhs, Fixed rhs) { return Fixed::fromRaw(lhs.value + rhs.value); }
        friend Fixed operator-(Fixed lhs, Fixed rhs) { return Fixed::fromRaw(lhs.value - rhs.value); }
        friend Fixed operator*(Fixed lhs, Fixed rhs) {
            return Fixed::fromRaw((static_cast<int64_t>(lhs.value) * rhs.value) >> K);
        }
        friend Fixed operator/(Fixed lhs, Fixed rhs) {
            return Fixed::fromRaw((static_cast<int64_t>(lhs.value) << K) / rhs.value);
        }

        Fixed& operator+=(Fixed rhs) { value += rhs.value; return *this; }
        Fixed& operator-=(Fixed rhs) { value -= rhs.value; return *this; }
        Fixed& operator*=(Fixed rhs) {
            value = (static_cast<int64_t>(value) * rhs.value) >> K;
            return *this;
        }
        Fixed& operator/=(Fixed rhs) {
            value = (static_cast<int64_t>(value) << K) / rhs.value;
            return *this;
        }

        friend std::ostream& operator<<(std::ostream& os, const Fixed& obj) {
            return os << static_cast<double>(obj);
        }

        static constexpr Fixed abs(Fixed x) { return x.value < 0 ? Fixed::fromRaw(-x.value) : x; }
    };

// Type alias for FastFixed
    template <std::size_t N, std::size_t K>
    using FastFixed = Fixed<N, K, true>;

// Mixed arithmetic operators
#define DEFINE_MIXED_OPS(OP) \
    template <std::size_t N, std::size_t K> \
    Fixed<N, K> operator OP(const Fixed<N, K>& a, const FastFixed<N, K>& b) { \
        return a OP static_cast<Fixed<N, K>>(b); \
    } \
    template <std::size_t N, std::size_t K> \
    Fixed<N, K> operator OP(const FastFixed<N, K>& a, const Fixed<N, K>& b) { \
        return static_cast<Fixed<N, K>>(a) OP b; \
    }

    DEFINE_MIXED_OPS(+)
    DEFINE_MIXED_OPS(-)
    DEFINE_MIXED_OPS(*)
    DEFINE_MIXED_OPS(/)
#undef DEFINE_MIXED_OPS

// Mixed assignment operators
#define DEFINE_MIXED_ASSIGN_OPS(OP) \
    template <std::size_t N, std::size_t K> \
    Fixed<N, K>& operator OP(Fixed<N, K>& a, const FastFixed<N, K>& b) { \
        return a OP static_cast<Fixed<N, K>>(b); \
    }

    DEFINE_MIXED_ASSIGN_OPS(+=)
    DEFINE_MIXED_ASSIGN_OPS(-=)
    DEFINE_MIXED_ASSIGN_OPS(*=)
    DEFINE_MIXED_ASSIGN_OPS(/=)
#undef DEFINE_MIXED_ASSIGN_OPS

// Mixed comparison operators
#define DEFINE_MIXED_COMPARISON(OP) \
    template <std::size_t N, std::size_t K> \
    bool operator OP(const Fixed<N, K>& a, const FastFixed<N, K>& b) { \
        return a OP static_cast<Fixed<N, K>>(b); \
    } \
    template <std::size_t N, std::size_t K> \
    bool operator OP(const FastFixed<N, K>& a, const Fixed<N, K>& b) { \
        return static_cast<Fixed<N, K>>(a) OP b; \
    }

    DEFINE_MIXED_COMPARISON(<)
    DEFINE_MIXED_COMPARISON(<=)
    DEFINE_MIXED_COMPARISON(>)
    DEFINE_MIXED_COMPARISON(>=)
    DEFINE_MIXED_COMPARISON(==)
#undef DEFINE_MIXED_COMPARISON

}

#ifndef TYPES
#define TYPES FLOAT,FIXED(31,17),FAST_FIXED(25, 11),FIXED(32, 16),DOUBLE,FAST_FIXED(32, 16)
#endif

#ifndef SIZES
#define SIZES S(36,84), S(100,100)
#endif

using namespace types;
using namespace std;

constexpr size_t DEFAULT_N = 36, DEFAULT_M = 84;

string get_field_from_file(const string& filename) {
    ifstream file(filename);
    if (!file.is_open()) {
        throw runtime_error("Failed to open file: " + filename);
    }

    string content, line;
    while (getline(file, line)) {
        content += line + '\n';
    }
    return content;
}

struct Size {
    size_t n, m;
    constexpr Size(size_t n_, size_t m_) : n(n_), m(m_) {}
    constexpr Size() : n(0), m(0) {}
};

Size parse_size(const string& size_str) {
    if (!size_str.starts_with("S(")) {
        throw std::runtime_error("Size must start with S(");
    }

    size_t start = 2;
    size_t comma = size_str.find(',', start);
    if (comma == string::npos) {
        throw std::runtime_error("Invalid size format: missing comma");
    }

    size_t end = size_str.find(')', comma);
    if (end == string::npos) {
        throw std::runtime_error("Invalid size format: missing )");
    }

    size_t n = stoul(size_str.substr(start, comma - start));
    size_t m = stoul(size_str.substr(comma + 1, end - comma - 1));

    return Size(n, m);
}

template<Size... Sizes>
struct SizesList {
    static constexpr size_t size = sizeof...(Sizes);
    template<size_t I>
    static constexpr Size get() {
        constexpr Size arr[] = {Sizes...};
        return arr[I];
    }
};

template<typename List, size_t I = 0>
constexpr bool matches_size_impl(const Size& size) {
    if constexpr (I >= List::size) {
        return false;
    } else {
        return (List::template get<I>().n == size.n && List::template get<I>().m == size.m) ||
               matches_size_impl<List, I + 1>(size);
    }
}

template<typename List>
bool matches_size(const Size& size) {
    return matches_size_impl<List>(size);
}

template<typename NumericType, size_t N, size_t M>
struct VectorField {
    static constexpr std::array<pair<int, int>, 4> deltas{{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}};
    array<NumericType, 4> v[N][M];

    NumericType &add(int x, int y, int dx, int dy, NumericType dv) {
        return get(x, y, dx, dy) += dv;
    }

    NumericType &get(int x, int y, int dx, int dy) {
        size_t i = ranges::find(deltas, pair(dx, dy)) - deltas.begin();
        assert(i < deltas.size());
        return v[x][y][i];
    }
};

// Вспомогательная структура для static_assert
template<typename T>
struct dependent_false : std::false_type {};

// Проверка фиксированных типов
template<typename T>
struct is_fixed : std::false_type {};

template<std::size_t N, std::size_t K, bool Fast>
struct is_fixed<types::Fixed<N, K, Fast>> : std::true_type {};

template<typename T>
inline constexpr bool is_fixed_v = is_fixed<T>::value;


template<
        typename PressureType,
        typename VelocityType,
        typename VFlowType,
        size_t N = DEFAULT_N,
        size_t M = DEFAULT_M
>
struct SimulationState {
    PressureType rho[256];
    PressureType p[N][M]{};
    PressureType old_p[N][M]{};

    char field[N][M + 1];
    int last_use[N][M]{};
    int UT{0};
    mt19937 rnd{1337};
    int dirs[N][M]{};

    VectorField<VelocityType, N, M> velocity{};
    VectorField<VFlowType, N, M> velocity_flow{};

    SimulationState(const string& field_content) {
        stringstream ss(field_content);
        string line;
        size_t row = 0;
        while (getline(ss, line) && row < N) {
            size_t col = 0;
            while (col < M && col < line.length()) {
                field[row][col] = line[col];
                col++;
            }
            field[row][col] = '\0';
            row++;
        }
    }
};

template<typename T>
struct NumericTraits {
    static T from_raw(int32_t x) { return T(x) / T(1 << 16); }
};

template<size_t N, size_t K>
struct NumericTraits<Fixed<N,K>> {
    static Fixed<N,K> from_raw(typename Fixed<N,K>::StorageType x) {
        return Fixed<N,K>::from_raw(x);
    }
};

template<size_t N, size_t K>
struct is_fixed<Fixed<N,K>> : std::true_type {};

template<typename T>
struct is_fast_fixed : std::false_type {};

template<size_t N, size_t K>
struct is_fast_fixed<FastFixed<N,K>> : std::true_type {};

template<typename T>
inline constexpr bool is_fast_fixed_v = is_fast_fixed<T>::value;

template<typename PressureType, typename VelocityType, typename VFlowType, size_t N = DEFAULT_N, size_t M = DEFAULT_M>
class FluidSimulator {
private:
    static constexpr size_t T = 1'000'000;
    static constexpr std::array<pair<int, int>, 4> deltas{{{-1, 0}, {1, 0}, {0, -1}, {0, 1}}};

    char field[N][M + 1];
    PressureType p[N][M]{}, old_p[N][M];
    int last_use[N][M]{};
    int UT = 0;
    mt19937 rnd{1337};
    PressureType rho[256];

    struct VectorField {
        array<VelocityType, 4> v[N][M]{};

        VelocityType& get(int x, int y, int dx, int dy) {
            size_t i = ranges::find(deltas, pair(dx, dy)) - deltas.begin();
            assert(i < deltas.size());
            return v[x][y][i];
        }

        VelocityType& add(int x, int y, int dx, int dy, VelocityType dv) {
            return get(x, y, dx, dy) += dv;
        }
    };

    VectorField velocity{}, velocity_flow{};
    int dirs[N][M]{};

    struct ParticleParams {
        char type;
        PressureType cur_p;
        array<VelocityType, 4> v;

        void swap_with(FluidSimulator* sim, int x, int y) {
            swap(sim->field[x][y], type);
            swap(sim->p[x][y], cur_p);
            swap(sim->velocity.v[x][y], v);
        }
    };


    template<typename T>
    PressureType to_pressure(const T& value) {
        if constexpr (std::is_same_v<T, PressureType>) {
            // Если тип совпадает с PressureType, возвращаем как есть
            return value;
        } else if constexpr (std::is_floating_point_v<PressureType> || std::is_integral_v<PressureType>) {
            // Если PressureType — встроенный тип (float, double, int)
            return static_cast<PressureType>(value);
        } else if constexpr (std::is_floating_point_v<T> || std::is_integral_v<T>) {
            // Для float, double, int и других встроенных типов
            return PressureType::fromRaw(static_cast<typename PressureType::ValueType>(
                                                 value * (1 << PressureType::FractionBits)));
        } else if constexpr (is_fixed_v<T> && is_fixed_v<PressureType>) {
            // Для фиксированных точек (оба типа — Fixed)
            constexpr auto scale = (1 << PressureType::FractionBits) / (1 << T::FractionBits);
            return PressureType::fromRaw(static_cast<typename PressureType::ValueType>(
                                                 value.value * scale)); // Используем поле `value`
        } else {
            static_assert(dependent_false<T>::value, "Unsupported type for to_pressure");
        }
    }

    template<typename T>
    VelocityType to_velocity(const T& value) {
        if constexpr (std::is_same_v<T, VelocityType>) {
            // Если тип совпадает с VelocityType, возвращаем как есть
            return value;
        } else if constexpr (std::is_floating_point_v<VelocityType> || std::is_integral_v<VelocityType>) {
            // Если VelocityType — встроенный тип (float, double, int)
            return static_cast<VelocityType>(value);
        } else if constexpr (std::is_floating_point_v<T> || std::is_integral_v<T>) {
            // Для float, double, int и других встроенных типов
            return VelocityType::fromRaw(static_cast<typename VelocityType::ValueType>(
                                                 value * (1 << VelocityType::FractionBits)));
        } else if constexpr (is_fixed_v<T> && is_fixed_v<VelocityType>) {
            // Для фиксированных точек (оба типа — Fixed)
            constexpr auto scale = (1 << VelocityType::FractionBits) / (1 << T::FractionBits);
            return VelocityType::fromRaw(static_cast<typename VelocityType::ValueType>(
                                                 value.value * scale)); // Используем поле `value`
        } else {
            static_assert(dependent_false<T>::value, "Unsupported type for to_velocity");
        }
    }


    template<typename T>
    VFlowType to_flow(T value) {
        if constexpr (std::is_same_v<T, VFlowType>) {
            return value;
        } else {
            return VFlowType(value);
        }
    }

    tuple<VelocityType, bool, pair<int, int>> propagate_flow(int x, int y, VelocityType lim) {
        last_use[x][y] = UT - 1;
        VelocityType ret = 0;
        for (auto [dx, dy] : deltas) {
            int nx = x + dx, ny = y + dy;
            if (field[nx][ny] != '#' && last_use[nx][ny] < UT) {
                auto cap = velocity.get(x, y, dx, dy);
                auto flow = velocity_flow.get(x, y, dx, dy);
                if (flow == cap) continue;

                auto vp = min(lim, cap - flow);
                if (last_use[nx][ny] == UT - 1) {
                    velocity_flow.add(x, y, dx, dy, vp);
                    last_use[x][y] = UT;
                    return {vp, true, {nx, ny}};
                }
                auto [t, prop, end] = propagate_flow(nx, ny, vp);
                ret += t;
                if (prop) {
                    velocity_flow.add(x, y, dx, dy, t);
                    last_use[x][y] = UT;
                    return {t, prop && end != pair(x, y), end};
                }
            }
        }
        last_use[x][y] = UT;
        return {ret, false, {0, 0}};
    }

    VelocityType random01() {
        return VelocityType(static_cast<double>(rnd() & ((1 << 16) - 1)) / (1 << 16));
    }

    void propagate_stop(int x, int y, bool force = false) {
        if (!force) {
            bool stop = true;
            for (auto [dx, dy] : deltas) {
                int nx = x + dx, ny = y + dy;
                if (field[nx][ny] != '#' && last_use[nx][ny] < UT - 1 && velocity.get(x, y, dx, dy) > VelocityType(0)) {
                    stop = false;
                    break;
                }
            }
            if (!stop) return;
        }
        last_use[x][y] = UT;
        for (auto [dx, dy] : deltas) {
            int nx = x + dx, ny = y + dy;
            if (field[nx][ny] == '#' || last_use[nx][ny] == UT || velocity.get(x, y, dx, dy) > VelocityType(0)) continue;
            propagate_stop(nx, ny);
        }
    }

    VelocityType move_prob(int x, int y) {
        VelocityType sum = 0;
        for (auto [dx, dy] : deltas) {
            int nx = x + dx, ny = y + dy;
            if (field[nx][ny] == '#' || last_use[nx][ny] == UT) continue;
            auto v = velocity.get(x, y, dx, dy);
            if (v < VelocityType(0)) continue;
            sum += v;
        }
        return sum;
    }

    bool propagate_move(int x, int y, bool is_first) {
        last_use[x][y] = UT - is_first;
        bool ret = false;
        int nx = -1, ny = -1;
        do {
            array<VelocityType, 4> tres;
            VelocityType sum = 0;
            for (size_t i = 0; i < deltas.size(); ++i) {
                auto [dx, dy] = deltas[i];
                int nx = x + dx, ny = y + dy;
                if (field[nx][ny] == '#' || last_use[nx][ny] == UT) {
                    tres[i] = sum;
                    continue;
                }
                auto v = velocity.get(x, y, dx, dy);
                if (v < VelocityType(0)) {
                    tres[i] = sum;
                    continue;
                }
                sum += v;
                tres[i] = sum;
            }

            if (sum == VelocityType(0)) break;

            VelocityType p = random01() * sum;
            size_t d = ranges::upper_bound(tres, p) - tres.begin();

            auto [dx, dy] = deltas[d];
            nx = x + dx;
            ny = y + dy;
            assert(velocity.get(x, y, dx, dy) > VelocityType(0) && field[nx][ny] != '#' && last_use[nx][ny] < UT);

            ret = (last_use[nx][ny] == UT - 1 || propagate_move(nx, ny, false));
        } while (!ret);

        last_use[x][y] = UT;
        for (auto [dx, dy] : deltas) {
            int nx = x + dx, ny = y + dy;
            if (field[nx][ny] != '#' && last_use[nx][ny] < UT - 1 && velocity.get(x, y, dx, dy) < VelocityType(0)) {
                propagate_stop(nx, ny);
            }
        }
        if (ret && !is_first) {
            ParticleParams pp{};
            pp.swap_with(this, x, y);
            pp.swap_with(this, nx, ny);
            pp.swap_with(this, x, y);
        }
        return ret;
    }

public:
    FluidSimulator(const SimulationState<PressureType, VelocityType, VFlowType, N, M>& state) {
        memcpy(field, state.field, sizeof(field));
        rho[' '] = PressureType(0.01);
        rho['.'] = PressureType(1000);

        for (size_t x = 0; x < N; ++x) {
            for (size_t y = 0; y < M; ++y) {
                if (field[x][y] == '#') continue;
                for (auto [dx, dy] : deltas) {
                    dirs[x][y] += (field[x + dx][y + dy] != '#');
                }
            }
        }
    }

    void run() {
        PressureType g = PressureType(0.1);

        for (size_t i = 0; i < T; ++i) {
            PressureType total_delta_p = 0;

            // Apply external forces
            for (size_t x = 0; x < N; ++x) {
                for (size_t y = 0; y < M; ++y) {
                    if (field[x][y] == '#') continue;
                    if (field[x + 1][y] != '#')
                        velocity.add(x, y, 1, 0, VelocityType(g));
                }
            }

            // Apply forces from p
            memcpy(old_p, p, sizeof(p));
            for (size_t x = 0; x < N; ++x) {
                for (size_t y = 0; y < M; ++y) {
                    if (field[x][y] == '#') continue;
                    for (auto [dx, dy] : deltas) {
                        int nx = x + dx, ny = y + dy;
                        if (field[nx][ny] != '#' && old_p[nx][ny] < old_p[x][y]) {
                            auto delta_p = old_p[x][y] - old_p[nx][ny];
                            auto force = to_pressure(delta_p);
                            auto& contr = velocity.get(nx, ny, -dx, -dy);
                            if (to_pressure(contr) * rho[(int)field[nx][ny]] >= force) {
                                contr -= to_velocity(force / rho[(int)field[nx][ny]]);
                                continue;
                            }
                            force -= to_pressure(contr) * rho[(int)field[nx][ny]];
                            contr = 0;
                            velocity.add(x, y, dx, dy, to_velocity(force / rho[(int)field[x][y]]));
                            p[x][y] -= force / dirs[x][y];
                            total_delta_p -= force / dirs[x][y];
                        }
                    }
                }
            }

            // Make flow from velocities
            velocity_flow = {};
            bool prop = false;
            do {
                UT += 2;
                prop = false;
                for (size_t x = 0; x < N; ++x) {
                    for (size_t y = 0; y < M; ++y) {
                        if (field[x][y] != '#' && last_use[x][y] != UT) {
                            auto [t, local_prop, _] = propagate_flow(x, y, VelocityType(1));
                            if (t > VelocityType(0)) prop = true;
                        }
                    }
                }
            } while (prop);

            // Recalculate p with kinetic energy
            for (size_t x = 0; x < N; ++x) {
                for (size_t y = 0; y < M; ++y) {
                    if (field[x][y] == '#') continue;
                    for (auto [dx, dy] : deltas) {
                        auto old_v = velocity.get(x, y, dx, dy);
                        auto new_v = velocity_flow.get(x, y, dx, dy);
                        if (old_v > VelocityType(0)) {
                            assert(new_v <= old_v);
                            velocity.get(x, y, dx, dy) = new_v;
                            auto force = to_pressure(old_v - new_v) * rho[(int)field[x][y]];
                            if (field[x][y] == '.') force *= PressureType(0.8);
                            if (field[x + dx][y + dy] == '#') {
                                p[x][y] += force / dirs[x][y];
                                total_delta_p += force / dirs[x][y];
                            } else {
                                p[x + dx][y + dy] += force / dirs[x + dx][y + dy];
                                total_delta_p += force / dirs[x + dx][y + dy];
                            }
                        }
                    }
                }
            }

            UT += 2;
            prop = false;
            for (size_t x = 0; x < N; ++x) {
                for (size_t y = 0; y < M; ++y) {
                    if (field[x][y] != '#' && last_use[x][y] != UT) {
                        if (random01() < move_prob(x, y)) {
                            prop = true;
                            propagate_move(x, y, true);
                        } else {
                            propagate_stop(x, y, true);
                        }
                    }
                }
            }

            if (prop) {
                cout << "Tick " << i << ":\n";
                for (size_t x = 0; x < N; ++x) {
                    cout << field[x] << "\n";
                }
            }
        }
    }
};

template<typename P, typename V, typename VF>
void run_simulation(size_t n, size_t m, const string& field_content) {
    SimulationState<P, V, VF, DEFAULT_N, DEFAULT_M> state(field_content);
    FluidSimulator<P, V, VF, DEFAULT_N, DEFAULT_M> simulator(state);
    simulator.run();
}

template<typename T>
std::string get_pretty_type_name() {
    if constexpr (std::is_same_v<T, float>) {
        return "float";
    } else if constexpr (std::is_same_v<T, double>) {
        return "double";
    } else if constexpr (is_fixed_v<T>) {
        return "Fixed<" + std::to_string(T::TotalBits) + "," + std::to_string(T::FractionBits) + ">";
    } else if constexpr (is_fast_fixed_v<T>) {
        return "FastFixed<" + std::to_string(T::TotalBits) + "," + std::to_string(T::FractionBits) + ">";
    } else {
        return "unknown";
    }
}

pair<size_t, size_t> parse_fixed_params(const string& type) {
    size_t start = type.find('(') + 1;
    size_t comma = type.find(',', start);
    size_t end = type.find(')', comma);

    size_t N = stoul(type.substr(start, comma - start));
    size_t K = stoul(type.substr(comma + 1, end - comma - 1));
    return {N, K};
}

template<typename T>
static bool matches_type(const string& type) {
    cerr << "Checking type: " << type << " against template type: " << get_pretty_type_name<T>() << endl;

    if constexpr (std::is_same_v<T, float>) {
        return type == "FLOAT";
    } else if constexpr (std::is_same_v<T, double>) {
        return type == "DOUBLE";
    } else if constexpr (is_fixed_v<T> && !is_fast_fixed_v<T>) {
        if (!type.starts_with("FIXED(")) return false;
        auto [bits, frac] = parse_fixed_params(type);
        return bits == T::TotalBits && frac == T::FractionBits;
    } else if constexpr (is_fast_fixed_v<T>) {
        if (!type.starts_with("FAST_FIXED(")) return false;
        auto [bits, frac] = parse_fixed_params(type);
        return bits == T::TotalBits && frac == T::FractionBits;
    }
    return false;
}

template<typename... Types>
struct TypesList {
    static constexpr size_t size = sizeof...(Types);
    template<size_t I>
    using type_at = typename std::tuple_element<I, std::tuple<Types...>>::type;
};

template<typename AllowedTypes, typename SelectedTypes = void>
struct TypeSelector {
    template<typename... Selected>
    static bool try_combinations(const string& p_type, const string& v_type, const string& v_flow_type,
                                 const Size& size, const string& field_content) {
        return try_all_p_types<0>(p_type, v_type, v_flow_type, size, field_content);
    }

private:
    template<size_t I>
    static bool try_all_p_types(const string& p_type, const string& v_type, const string& v_flow_type,
                                const Size& size, const string& field_content) {
        if constexpr (I >= AllowedTypes::size) {
            return false;
        } else {
            using P = typename AllowedTypes::template type_at<I>;
            return try_with_p_type<P>(p_type, v_type, v_flow_type, size, field_content) ||
                   try_all_p_types<I + 1>(p_type, v_type, v_flow_type, size, field_content);
        }
    }

    template<typename P>
    static bool try_with_p_type(const string& p_type, const string& v_type, const string& v_flow_type,
                                const Size& size, const string& field_content) {
        if (!matches_type<P>(p_type)) return false;
        return try_all_v_types<P, 0>(v_type, v_flow_type, size, field_content);
    }

    template<typename P, size_t I>
    static bool try_all_v_types(const string& v_type, const string& v_flow_type,
                                const Size& size, const string& field_content) {
        if constexpr (I >= AllowedTypes::size) {
            return false;
        } else {
            using V = typename AllowedTypes::template type_at<I>;
            return try_with_v_type<P, V>(v_type, v_flow_type, size, field_content) ||
                   try_all_v_types<P, I + 1>(v_type, v_flow_type, size, field_content);
        }
    }

    template<typename P, typename V>
    static bool try_with_v_type(const string& v_type, const string& v_flow_type,
                                const Size& size, const string& field_content) {
        if (!matches_type<V>(v_type)) return false;
        return try_all_vf_types<P, V, 0>(v_flow_type, size, field_content);
    }

    template<typename P, typename V, size_t I>
    static bool try_all_vf_types(const string& v_flow_type, const Size& size, const string& field_content) {
        if constexpr (I >= AllowedTypes::size) {
            return false;
        } else {
            using VF = typename AllowedTypes::template type_at<I>;
            return try_with_vf_type<P, V, VF>(v_flow_type, size, field_content) ||
                   try_all_vf_types<P, V, I + 1>(v_flow_type, size, field_content);
        }
    }

    template<typename P, typename V, typename VF>
    static bool try_with_vf_type(const string& v_flow_type, const Size& size, const string& field_content) {
        cerr << "Checking combination: P=" << get_pretty_type_name<P>()
             << ", V=" << get_pretty_type_name<V>()
             << ", VF=" << get_pretty_type_name<VF>() << endl;

        if (!matches_type<VF>(v_flow_type)) {
            cerr << "VF type mismatch: " << v_flow_type << " does not match " << get_pretty_type_name<VF>() << endl;
            return false;
        }

        cerr << "Combination matched! Running simulation..." << endl;
        run_simulation<P, V, VF>(size.n, size.m, field_content);
        return true;
    }
};


template<typename... Types>
bool try_all_type_combinations(const string& p_type, const string& v_type, const string& v_flow_type,
                               const Size& size, const string& field_content) {
    return TypeSelector<TypesList<Types...>, TypesList<>>::try_combinations(p_type, v_type, v_flow_type, size, field_content);
}

bool create_and_run_simulation(const string& p_type, const string& v_type, const string& v_flow_type,
                               const Size& size, const string& field_content) {
    try {
        cerr << "\nTrying to create simulation with:" << endl;
        cerr << "p_type: " << p_type << endl;
        cerr << "v_type: " << v_type << endl;
        cerr << "v_flow_type: " << v_flow_type << endl;
        cerr << "size: S(" << size.n << "," << size.m << ")" << endl;

#define S(N, M) Size(N, M)
        using SizesListType = SizesList<SIZES>;
#undef S

        if (!matches_size<SizesListType>(size)) {
            cerr << "Error: Unsupported size" << endl;
            return false;
        }

#define FLOAT float
#define DOUBLE double
#define FIXED(N, K) Fixed<N, K>
#define FAST_FIXED(N, K) FastFixed<N, K>
        if (!try_all_type_combinations<TYPES>(p_type, v_type, v_flow_type, size, field_content)) {
            cerr << "Error: No matching type combination found" << endl;
            return false;
        }
        return true;
    }
    catch (const std::exception& e) {
        cerr << "Error: " << e.what() << endl;
        return false;
    }
}

string get_arg(string_view arg_name, int argc, char** argv, string_view default_value) {
    for (int i = 1; i < argc - 1; ++i) {
        if (argv[i] == arg_name) {
            return argv[i + 1];
        }
    }
    return string(default_value);
}

bool is_valid_type(const string& type) {
    if (type == "FLOAT" || type == "DOUBLE") return true;

    if (type.starts_with("FIXED(") || type.starts_with("FAST_FIXED(")) {
        size_t pos = type.find(',');
        if (pos == string::npos) return false;

        size_t end_pos = type.find(')', pos);
        if (end_pos == string::npos) return false;

        return true;
    }

    return false;
}

Size get_field_size(const string& field_content) {
    size_t n = 0, m = 0;
    stringstream ss(field_content);
    string line;

    while (getline(ss, line)) {
        if (m == 0) m = line.length();
        else if (line.length() != m) {
            throw runtime_error("Invalid field format: lines have different lengths");
        }
        n++;
    }

    if (n == 0 || m == 0) {
        throw runtime_error("Empty field");
    }

    if (n > DEFAULT_N || m > DEFAULT_M) {
        throw runtime_error("Field size " + to_string(n) + "x" + to_string(m) +
                            " exceeds maximum allowed size " + to_string(DEFAULT_N) +
                            "x" + to_string(DEFAULT_M));
    }

    return Size(n, m);
}

int main(int argc, char** argv) {
    string p_type = get_arg("--p-type", argc, argv, "FAST_FIXED(32, 16)");
    string v_type = get_arg("--v-type", argc, argv, "FIXED(32, 16)");
    string v_flow_type = get_arg("--v-flow-type", argc, argv, "FAST_FIXED(32, 16)");
    string field_file = get_arg("--field", argc, argv, "fluid.txt");

    string field_content;
    try {
        field_content = get_field_from_file(field_file);
    } catch (const exception& e) {
        cerr << "Error reading field file: " << e.what() << endl;
        return 1;
    }

    Size size;
    try {
        size = get_field_size(field_content);
        cerr << "Field size: " << size.n << "x" << size.m << endl;
    } catch (const exception& e) {
        cerr << "Error validating field: " << e.what() << endl;
        return 1;
    }

    if (!is_valid_type(p_type)) {
        cerr << "Invalid p_type: " << p_type << endl;
        return 1;
    }
    if (!is_valid_type(v_type)) {
        cerr << "Invalid v_type: " << v_type << endl;
        return 1;
    }
    if (!is_valid_type(v_flow_type)) {
        cerr << "Invalid v_flow_type: " << v_flow_type << endl;
        return 1;
    }

    if (!create_and_run_simulation(p_type, v_type, v_flow_type, size, field_content)) {
        cerr << "Failed to create simulation" << endl;
        return 1;
    }

    return 0;
}
