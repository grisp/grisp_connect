# Client

## State Diagram

```mermaid
stateDiagram
  direction TB
  [*] --> init
  init --> idle:connect = false
  init --> waiting_ip:connect = true
  idle --> waiting_ip:connect()
  idle --> rebooting:reboot()
  waiting_ip --> disconnected:ip available
  waiting_ip --> idle:disconnect()
  waiting_ip --> rebooting:reboot()
  disconnected --> connecting:after exponential backoff timeout
  disconnected --> idle:disconnect()
  disconnected --> rebooting:reboot()
  connecting --> waiting_ip:timeout / close_connection()
  connecting --> waiting_ip:connection died
  connecting --> handshake:connected()
  connecting --> idle:disconnect() / close_connection()
  connecting --> rebooting:reboot() / close_connection()
  handshake --> waiting_ip:timeout / close_connection()
  handshake --> waiting_ip:connection died
  handshake --> unlinked:Handshake() response with linked = false
  handshake --> linked:Handshake() response with linked = true
  handshake --> idle:disconnect() / close_connection()
  handshake --> rebooting:reboot() / close_connection()
  unlinked --> waiting_ip:connection died
  unlinked --> linked:LinkDevice() succeed
  unlinked --> unlinked:LinkDevice() failed
  unlinked --> idle:disconnect() / close_connection()
  unlinked --> rebooting:reboot() / close_connection()
  linked --> unlinked:DeviceUnlinked()
  linked --> waiting_ip:connection died
  linked --> idle:disconnect() / close_connection()
  linked --> rebooting:reboot() / close_connection()
  rebooting --> [*]:  
```
