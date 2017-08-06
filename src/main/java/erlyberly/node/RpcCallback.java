package erlyberly.node;

public interface RpcCallback<T> {
    void callback(T result);
}
