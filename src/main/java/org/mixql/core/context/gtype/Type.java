package org.mixql.core.context.gtype;

public abstract class Type {
    // +
    public Type Add(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )

        );
    }

    // -
    public Type Subtract(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // *
    public Type Multiply(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // /
    public Type Divide(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // ||
    public Type Or(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // &&
    public Type And(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // <
    public Type LessThen(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // <=
    public Type LessEqualThen(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // >
    public Type MoreThen(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // >=
    public Type MoreEqualThen(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // ==
    public Type Equal(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // !=
    public Type NotEqual(Type other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // !=
    public Type Not() {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: operation `not` for %s is unsupported",
                        this.getClass().getSimpleName()
                )
        );
    }
}
