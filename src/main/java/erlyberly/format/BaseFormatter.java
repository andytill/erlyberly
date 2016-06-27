package erlyberly.format;

public abstract class BaseFormatter implements TermFormatter {

    @Override
    public String emptyTupleString() {
        return "#( )";
    }

    @Override
    public String tupleLeftParen() {
        return "#(";
    }

    @Override
    public String tupleRightParen() {
        return ")";
    }

    @Override
    public String emptyListString() {
        return "( )";
    }

    @Override
    public String listLeftParen() {
        return ")";
    }

    @Override
    public String listRightParen() {
        return ")";
    }
}
