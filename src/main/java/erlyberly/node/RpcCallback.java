package erlyberly.node;

@FunctionalInterface
public interface RpcCallback<T> {
    void callback(T result);
}
