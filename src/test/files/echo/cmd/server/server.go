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
	listenAddr := flag.String("listenAddr", "<required>", "hostname:port pair to listen on")
	flag.Parse()

	if *tracerAddr == "<required>" || *listenAddr == "<required>" {
		flag.Usage()
		os.Exit(1)
	}

	tracer := tracing.NewTracer(tracing.TracerConfig{
		ServerAddress: *tracerAddr,
		TracerIdentity: "echo_server",
	})

	coreTrace := tracer.CreateTrace()
	coreTrace.RecordAction(echo.ServerStart{})
	defer coreTrace.RecordAction(echo.ServerStop{})

	listener, err := net.Listen("tcp", *listenAddr)
	if err != nil {
		panic(err)
	}
	for {
		conn, err := listener.Accept()
		if err != nil {
			panic(err)
		}
		dec := gob.NewDecoder(conn)
		enc := gob.NewEncoder(conn)

		var reqMsg echo.RequestMsg
		err = dec.Decode(&reqMsg)
		if err != nil {
			panic(err)
		}
		connTrace := tracer.ReceiveToken(reqMsg.Token)
		connTrace.RecordAction(echo.ServerEcho {
			Kill: reqMsg.Kill,
		})

		err = enc.Encode(echo.ResponseMsg{
			Token: coreTrace.GenerateToken(),
		})
		if err != nil {
			panic(err)
		}

		err = conn.Close()
		if err != nil {
			panic(err)
		}

		if reqMsg.Kill {
			break
		}
	}
}
