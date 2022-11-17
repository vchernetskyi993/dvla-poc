package com.example.pub

import nl.topicus.overheid.kamel.route.rest.get
import nl.topicus.overheid.kamel.route.rest.post
import nl.topicus.overheid.kamel.route.rest.rest
import org.apache.camel.builder.RouteBuilder

fun RouteBuilder.configureApi() {
    restConfiguration()
        .port(System.getenv("SERVER_PORT") ?: restConfiguration.port)

    rest("/proofs") {
        post {
            produces("application/json")
            to("direct:createProofRequest")
        }
        get("/{requestId}") {
            produces("application/json")
            to("direct:presentProofRequest")
        }
    }

    rest("/webhooks/topic/{topic}") {
        post {
            consumes("application/json")
            to("direct:webhook")
        }
    }

    from("direct:webhook")
        .log("Received \${headers.topic}: \${body}")
}
