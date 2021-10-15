package main

import (
	"flag"
	"github.com/DistributedClocks/tracing"
	"os"
)

func main() {
	serverBind := flag.String("serverBind", "<required>", "hostname:port to listen on")
	outFile := flag.String("outFile", "trace_log.txt", "log file to write to")
	shivizOutFile := flag.String("shivizOutFile", "trace_log_shiviz.txt", "additional output file for ShiViz-format logs")

	flag.Parse()

	if *serverBind == "<required>" {
		flag.Usage()
		os.Exit(1)
	}

	server := tracing.NewTracingServer(tracing.TracingServerConfig{
		ServerBind:       *serverBind,
		OutputFile:       *outFile,
		ShivizOutputFile: *shivizOutFile,
	})

	err := server.Open()
	if err != nil {
		panic(err)
	}
	defer func() {
		err := server.Close()
		if err != nil {
			panic(err)
		}
	}()

	server.Accept()
}
