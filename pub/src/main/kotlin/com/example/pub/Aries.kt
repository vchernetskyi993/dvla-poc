package com.example.pub

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import nl.topicus.overheid.kamel.getBody
import org.apache.camel.builder.RouteBuilder
import org.hyperledger.acy_py.generated.model.DID
import org.hyperledger.acy_py.generated.model.DIDEndpoint
import org.hyperledger.aries.api.present_proof.PresentProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest.ProofRequestedAttributes
import org.hyperledger.aries.api.present_proof.PresentationExchangeRecord

fun RouteBuilder.configureAries() {
    from("direct:createProofRequest")
        .setBody {
            PresentProofRequest().apply {
                autoVerify = true
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
        .enrich("direct:serviceDecorator") { old, new ->
            val input = old.getIn().getBody(PresentationExchangeRecord::class)
            val decorator = new.getIn().getBody(JsonObject::class)

            old.getIn().body = input?.presentationRequestDict?.apply {
                add("~service", decorator)
            }
            old
        }

    from("direct:serviceDecorator")
        .to("hyperledger-aries:admin?service=/wallet/did/public")
        .setHeader("did", simple("\${body.did}"))
        .enrich("hyperledger-aries:admin?service=/wallet/get-did-endpoint") { old, new ->
            val agentDid = old.getIn().getBody(DID::class)
            val agentEndpoint = new.getIn().getBody(DIDEndpoint::class)

            old.getIn().body = JsonObject().apply {
                add("recipientKeys", JsonArray().apply { add(agentDid?.verkey) })
                addProperty("serviceEndpoint", agentEndpoint?.endpoint)
            }
            old
        }
}
