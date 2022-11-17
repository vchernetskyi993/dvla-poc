package com.example.pub

import nl.topicus.overheid.kamel.route.rest.get
import nl.topicus.overheid.kamel.route.rest.rest
import org.apache.camel.builder.RouteBuilder
import org.apache.camel.main.Main

class Application

fun main() = Main(Application::class.java).run()

class ApiRoutes : RouteBuilder() {
    override fun configure() {
        restConfiguration()
            .port(System.getenv("SERVER_PORT") ?: restConfiguration.port)

        rest("") {
            // TODO: endpoint to create proof request and return link to it
            // TODO: endpoint to return present-proof record
            get("/greeting") {
                to("direct:hello")
            }
        }

        from("direct:hello")
            .transform()
            .constant("Hello, World!")
    }
}
