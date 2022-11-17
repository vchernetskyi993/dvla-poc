package com.example.pub

import nl.topicus.overheid.kamel.route.rest.get
import nl.topicus.overheid.kamel.route.rest.post
import nl.topicus.overheid.kamel.route.rest.rest
import org.apache.camel.builder.RouteBuilder
import org.apache.camel.main.Main

class Application

fun main() = Main(Application::class.java).run()

class ApiRoutes : RouteBuilder() {
    override fun configure() {
        restConfiguration()
            .port(System.getenv("SERVER_PORT") ?: restConfiguration.port)

        rest("/proofs") {
            post("") {
                produces("application/json")
                to("direct:createProofRequest")
            }
            get("/{requestId}") {
                produces("application/json")
                to("direct:presentProofRequest")
            }
        }

        // TODO: create proof request and return link to it
        from("direct:createProofRequest")
            .transform()
            .simple("{\"url\": \"my-url\"}")

        // TODO: return present-proof record
        from("direct:presentProofRequest")
            .transform()
            .simple("{\"id\": \"\${headers.requestId}\"}")
    }
}
