package com.example.pub

import com.google.gson.JsonArray
import com.google.gson.JsonObject
import nl.topicus.overheid.kamel.getBody
import org.apache.camel.builder.RouteBuilder
import org.hyperledger.acy_py.generated.model.DID
import org.hyperledger.acy_py.generated.model.DIDEndpoint
import org.hyperledger.acy_py.generated.model.IndyProofReqPredSpec.PTypeEnum.LESS_THAN_OR_EQUAL_TO
import org.hyperledger.aries.api.present_proof.PresentProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest.ProofRequestedAttributes
import org.hyperledger.aries.api.present_proof.PresentProofRequest.ProofRequest.ProofRequestedPredicates
import org.hyperledger.aries.api.present_proof.PresentationExchangeRecord
import java.time.LocalDate
import java.time.format.DateTimeFormatter

private val schemaRestriction = JsonObject().apply {
    addProperty("schema_name", "driver_license")
}

fun RouteBuilder.configureAries() {
    from("direct:createProofRequest")
        .setBody {
            PresentProofRequest().apply {
                autoVerify = true
                proofRequest = ProofRequest().apply {
                    requestedAttributes = mapOf(
                        "0_first_name_uuid" to ProofRequestedAttributes().apply {
                            name = "first_name"
                            restrictions = listOf(schemaRestriction)
                        },
                    )
                    requestedPredicates = mapOf(
                        "0_dob_GE_uuid" to ProofRequestedPredicates().apply {
                            name = "dob"
                            pType = LESS_THAN_OR_EQUAL_TO
                            pValue = LocalDate.now()
                                .minusYears(18)
                                .format(DateTimeFormatter.ofPattern("yyyyMMdd"))
                                .toInt()
                            restrictions = listOf(schemaRestriction)
                        }
                    )
                }
            }
        }
        .to("hyperledger-aries:admin?service=/present-proof/create-request")
        .transform().simple("{\"url\": \"\${env.PUBLIC_URL}/proofs/\${body.presentationExchangeId}\"}")
        .setHeader("Content-Type", constant("application/json"))

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
        .setHeader("Content-Type", constant("application/json"))

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
