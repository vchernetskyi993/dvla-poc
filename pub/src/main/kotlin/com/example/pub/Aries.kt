package com.example.pub

import com.google.gson.JsonObject
import org.apache.camel.builder.RouteBuilder
import org.hyperledger.aries.api.present_proof.PresentProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest.ProofRequestedAttributes
import org.hyperledger.aries.api.present_proof.PresentationExchangeRecord

fun RouteBuilder.configureAries() {
    from("direct:createProofRequest")
        .setBody {
            PresentProofRequest().apply {
                proofRequest = ProofRequest().apply {
                    requestedAttributes = mapOf(
                        "0_first_name_uuid" to ProofRequestedAttributes().apply {
                            name = "first_name"
                            restrictions = listOf(
                                JsonObject().apply {
                                    addProperty("schema_name", "driver_license")
                                },
                            )
                        },
                    )
                    requestedPredicates = mapOf()
                }
            }
        }
        .to("hyperledger-aries:admin?service=/present-proof/create-request")
        .transform().simple("{\"url\": \"\${env.PUBLIC_URL}/proofs/\${body.presentationExchangeId}\"}")

    from("direct:presentProofRequest")
        .toD("hyperledger-aries:admin?service=/present-proof/records/\${headers.requestId}")
        .setBody {
            val input = it.getIn().getBody(PresentationExchangeRecord::class.java)
            // TODO: set ~service
            input.presentationRequestDict
        }
}
