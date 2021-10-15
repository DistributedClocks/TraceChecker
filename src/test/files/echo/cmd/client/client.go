package main

import (
	"encoding/gob"
	"example.com/echo"
	"flag"
	"github.com/DistributedClocks/tracing"
	"net"
	"os"
)

func main() {
	tracerAddr := flag.String("tracerAddr", "<required>", "the tracing server to dial")
	tracingId := flag.String("tracingId", "client", "the client's id for the tracer")
	dialAddr := flag.String("dialAddr", "<required>", "hostname:port pair to dial")
	requestCount := flag.Int("requestCount", -1, "number of requests to send (must be at least 1 if kill is set)")
	killOpt := flag.Bool("kill", false, "whether to ask the server to stop")
	flag.Parse()

	if *tracerAddr == "<required>" || *dialAddr == "<required>" || *requestCount < 0 {
		flag.Usage()
		os.Exit(1)
	}
	if *killOpt && *requestCount < 1 {
		flag.Usage()
		os.Exit(1)
	}

	tracer := tracing.NewTracer(tracing.TracerConfig{
		ServerAddress: *tracerAddr,
		TracerIdentity: *tracingId,
	})

	coreTrace := tracer.CreateTrace()
	coreTrace.RecordAction(echo.ClientStart{
		RequestCount: *requestCount,
		Kill: *killOpt,
	})
	defer coreTrace.RecordAction(echo.ClientStop{})

	nonKillRequests := *requestCount
	if *killOpt {
		nonKillRequests -= 1
	}

	for i := 0; i < nonKillRequests; i++ {
		func() {
			connTrace := tracer.CreateTrace()

			conn, err := net.Dial("tcp", *dialAddr)
			if err != nil {
				panic(err)
			}
			defer func() {
				err := conn.Close()
				if err != nil {
					panic(err)
				}
			}()
			enc := gob.NewEncoder(conn)
			dec := gob.NewDecoder(conn)

			connTrace.RecordAction(echo.ClientSend{})

			err = enc.Encode(echo.RequestMsg{
				Token: connTrace.GenerateToken(),
				Kill:  false,
			})
			if err != nil {
				panic(err)
			}

			var respMsg echo.ResponseMsg
			err = dec.Decode(&respMsg)
			if err != nil {
				panic(err)
			}

			connTrace = tracer.ReceiveToken(respMsg.Token)

			connTrace.RecordAction(echo.ClientReceive{})
		}()
	}

	if *killOpt {
		killTrace := tracer.CreateTrace()

		conn, err := net.Dial("tcp", *dialAddr)
		if err != nil {
			panic(err)
		}
		defer func() {
			err := conn.Close()
			if err != nil {
				panic(err)
			}
		}()
		enc := gob.NewEncoder(conn)
		dec := gob.NewDecoder(conn)

		killTrace.RecordAction(echo.ClientSend{})

		err = enc.Encode(echo.RequestMsg{
			Token: killTrace.GenerateToken(),
			Kill: true,
		})
		if err != nil {
			panic(err)
		}

		var respMsg echo.ResponseMsg
		err = dec.Decode(&respMsg)
		if err != nil {
			panic(err)
		}

		killTrace = tracer.ReceiveToken(respMsg.Token)

		killTrace.RecordAction(echo.ClientReceive{})
	}
}
