import axios from "axios";
import React from "react";
import QRCode from "react-qr-code";

function App() {
  const [url, setUrl] = React.useState("");

  const createProofRequest = () => {
    axios
      .post("/api/proofs")
      .then((response) => setUrl(response.data.url));
  };

  return (
    <div>
      <header>Pub</header>
      <div>
        <button onClick={createProofRequest}>Generate Proof Request</button>
      </div>
      {url && (
        <div style={{ padding: "16px" }}>
          <QRCode value={url} size={512} />
        </div>
      )}
    </div>
  );
}

export default App;
