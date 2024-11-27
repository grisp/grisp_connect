# Architecture

```mermaid
graph TD
    RootSup[VM Root Supervisor]
    
    subgraph GrispConnectApp[Grisp Connect Application]
        GrispConnectRootSup[Root Supervisor<br>grisp_connect_sup]
        GrispConnectLogServer[Log Server<br>grisp_connect_log_server]
        GrispConnectClient[Client<br>grisp_connect_client]
        GrispConnectConnection[JSON-RPC Connection<br>grisp_connect_connection]
        GrispConnectJsonRPC[JSON-RPC Codec<br>grisp_connect_jsonrpc]
        
        GrispConnectRootSup --Supervise--> GrispConnectLogServer
        GrispConnectRootSup --Supervise--> GrispConnectClient
        GrispConnectClient --Spawn and Monitor--> GrispConnectConnection
        GrispConnectConnection --Use--> GrispConnectJsonRPC
    end
    
    subgraph GunApp[Gun Application]
        GunRootSup[Gun Root Supervisor<br>gun_sup]
        GunConnsSup[Gun Connection Supervisor<br>gun_conns_sup]
        Gun[Gun Connection<br>gun]
        Gun[Gun HTTP Handler<br>gun_http]

        GunRootSup --Supervise--> GunConnsSup
        GunConnsSup --Supervise--> Gun
    end
    
    RootSup --Supervise--> GrispConnectRootSup
    RootSup --Supervise--> GunRootSup
    GrispConnectConnection -.Interact.-> Gun
```


## Client

The client process is the main state machine. Its responsabilities are:

 - Trigger connection/reconnection to the backend.
 - Expose high-level protocol API to the application.
 - Implement generic API endpoints.
 
See the [client documentation](grisp_connect_client.md).


## Connection

`grisp_connect_connection` module encpasulate a JSON-RPC connection.

It is not supervised, the process starting it must monitor it.

It provides a high-level API to a JSON-RPC connection:

 - Perform synchronous requests
 - Start asynchronous requests
 - Reply to a request
 - Send and error result for a request
 - Send asynchronous notifications
 - Send generic errors

When performing an asynchronous request, the caller can give an opaque context
term, that will given back when receiving a response or an error for this
request, allowing the caller to handle the asynchronous operation without
having to store information locally. 
