import axios from "axios";
import React from "react";
import QRCode from "react-qr-code";

function App() {
  const [url, setUrl] = React.useState("");

  const createInvitation = () => {
    axios
      .post("/api/invitations")
      .then((response) => setUrl(response.data.url));
  };

  return (
    <div>
      <header>Driver and Vehicle Licensing Agency</header>
      <div>
        <button onClick={createInvitation}>Generate Invitation</button>
      </div>
      <div style={{ padding: "16px" }}>
        <QRCode value={url} size={512} />
      </div>
    </div>
  );
}

export default App;
