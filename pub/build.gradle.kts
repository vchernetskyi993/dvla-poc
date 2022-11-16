plugins {
    application
    kotlin("jvm") version "1.7.21"
}

val camelVersion: String by project
val logbackVersion: String by project

repositories {
    mavenCentral()
}

application {
    mainClass.set("com.example.pub.AppKt")
}

dependencies {
    implementation("org.apache.camel:camel-core:$camelVersion")
    implementation("org.apache.camel:camel-main:$camelVersion")
    implementation("org.apache.camel:camel-netty-http:$camelVersion")

    implementation("nl.topicus.overheid:kamel:1.0")

    implementation("ch.qos.logback:logback-core:$logbackVersion")
    implementation("ch.qos.logback:logback-classic:$logbackVersion")
}
