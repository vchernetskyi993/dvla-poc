package com.example.pub

import org.apache.camel.builder.RouteBuilder
import org.apache.camel.main.Main

class Application

fun main() = Main(Application::class.java).run()

class AppRoutes : RouteBuilder() {
    override fun configure() {
        configureApi()
        configureAries()
    }
}
