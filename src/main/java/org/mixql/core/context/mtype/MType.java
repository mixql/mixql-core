package org.mixql.core.context.mtype;

public abstract class MType extends Exception {

    // +
    public MType Add(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )

        );
    }

    // -
    public MType Subtract(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // *
    public MType Multiply(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // /
    public MType Divide(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // ||
    public MType Or(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // &&
    public MType And(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // <
    public MType LessThen(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // <=
    public MType LessEqualThen(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // >
    public MType MoreThen(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // >=
    public MType MoreEqualThen(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // ==
    public MType Equal(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // !=
    public MType NotEqual(MType other) {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: %s + %s is unsupported",
                        this.getClass().getSimpleName(), other.getClass().getSimpleName()
                )
        );
    }

    // !=
    public MType Not() {
        throw new UnsupportedOperationException(
                String.format(
                        "type error: operation `not` for %s is unsupported",
                        this.getClass().getSimpleName()
                )
        );
    }
}
