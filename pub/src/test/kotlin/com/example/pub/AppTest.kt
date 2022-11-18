package com.example.pub

import io.restassured.RestAssured
import io.restassured.module.kotlin.extensions.Then
import io.restassured.module.kotlin.extensions.When
import org.junit.jupiter.api.AfterAll
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.mockserver.integration.ClientAndServer
import org.mockserver.integration.ClientAndServer.startClientAndServer
import org.mockserver.model.HttpRequest.request
import org.mockserver.model.HttpResponse.response
import org.mockserver.model.JsonBody.json
import org.mockserver.model.MediaType
import org.slf4j.LoggerFactory
import org.testcontainers.containers.GenericContainer
import org.testcontainers.containers.output.Slf4jLogConsumer
import org.testcontainers.images.builder.ImageFromDockerfile
import org.testcontainers.junit.jupiter.Container
import org.testcontainers.junit.jupiter.Testcontainers
import java.nio.file.Paths

private const val REQUEST_ID = "12334567"

@Testcontainers
class AppTest {

    private val mockServer: ClientAndServer = startClientAndServer()

    @Container
    val service: GenericContainer<*> = GenericContainer(
        ImageFromDockerfile().withFileFromPath("/", Paths.get("."))
    )
        .withExposedPorts(8080)
        .withLogConsumer(Slf4jLogConsumer(LoggerFactory.getLogger("Application")))
        .withEnv(
            mapOf(
                "ACAPY_HOSTNAME" to "172.17.0.1",
                "ACAPY_ADMIN_PORT" to "${mockServer.port}",
            )
        )

    @BeforeEach
    fun before() {
        println("Application port: ${service.firstMappedPort}")
        RestAssured.port = service.firstMappedPort
    }

    @AfterAll
    fun afterAll() {
        mockServer.stop()
    }

    @Test
    fun shouldCreateProofRequest() {
        mockServer.`when`(
            request()
                .withMethod("POST")
                .withPath("/present-proof/create-request")
                .withBody(
                    json(
                        """
                        {
                            "auto_verify": true, 
                            "proof_request": {
                                "name" : "Proof request",
                                "version" : "1.0",
                                "requested_attributes": {
                                    "0_first_name_uuid": {
                                        "name": "first_name",
                                        "restrictions": [
                                            {
                                                "schema_name": "driver_license"
                                            }
                                        ]
                                    }
                                },
                                "requested_predicates": {}
                            }
                        }""".trimIndent()
                    )
                )
        )
            .respond(
                response()
                    .withContentType(MediaType.APPLICATION_JSON)
                    .withBody("{\"presentation_exchange_id\": \"$REQUEST_ID\"}")
            )

        When {
            post("/proofs")
        } Then {
            statusCode(200)
            // FIXME: test assumes wrong content-type
//            body("url", equalTo("/proofs/$REQUEST_ID"))
        }
    }
}