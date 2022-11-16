package com.example.pub

import io.restassured.RestAssured
import io.restassured.module.kotlin.extensions.Then
import io.restassured.module.kotlin.extensions.When
import org.hamcrest.Matchers.`is`
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.testcontainers.containers.GenericContainer
import org.testcontainers.images.builder.ImageFromDockerfile
import org.testcontainers.junit.jupiter.Container
import org.testcontainers.junit.jupiter.Testcontainers
import java.nio.file.Paths

@Testcontainers
class AppTest {

    @Container
    val service: GenericContainer<*> = GenericContainer(
        ImageFromDockerfile().withFileFromPath("/", Paths.get("."))
    )
        .withExposedPorts(8080)

    @BeforeEach
    fun before() {
        RestAssured.port = service.firstMappedPort
    }

    @Test
    fun shouldSendGreeting() {
        When {
            get("/api/greeting")
        } Then {
            body(`is`("Hello, World!"))
        }
    }
}