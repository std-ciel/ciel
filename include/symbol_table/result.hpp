#ifndef RESULT_HPP
#define RESULT_HPP

#include <variant>


template <typename T, typename E> class Result {
  private:
    std::variant<T, E> data;

  public:
    Result(T value) : data(std::move(value)) {}
    Result(E error) : data(std::move(error)) {}

    bool is_ok() const
    {
        return std::holds_alternative<T>(data);
    }
    bool is_err() const
    {
        return std::holds_alternative<E>(data);
    }

    T &value()
    {
        return std::get<T>(data);
    }
    const T &value() const
    {
        return std::get<T>(data);
    }

    E &error()
    {
        return std::get<E>(data);
    }
    const E &error() const
    {
        return std::get<E>(data);
    }

    // Convenience operators
    explicit operator bool() const
    {
        return is_ok();
    }

    T &operator*()
    {
        return value();
    }
    const T &operator*() const
    {
        return value();
    }
};

#endif // RESULT_HPP
