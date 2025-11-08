const boxes = document.querySelectorAll('.round-box');
const modal = document.getElementById('modal2');
const modalImg = document.getElementById('modalImage');
const modalCode = document.getElementById('modalCode');
const modalText = document.getElementById('modalText');
const closeBtn = document.getElementById('closeBtn');

boxes.forEach(box => {
    box.addEventListener('click', async () => {
        const img = box.querySelector('img');
        modalImg.src = img.src;

        modalText.innerHTML = box.dataset.text || "";

        const codeFile = box.dataset.codefile;
        const codeLang = box.dataset.codelang || "js";

        if (codeFile) {
            try {
                const res = await fetch(codeFile);
                const codeText = await res.text();
                modalCode.textContent = codeText;

                modalCode.className = `language-${codeLang}`;
                Prism.highlightElement(modalCode);

            } catch (err) {
                console.log(err)
                modalCode.textContent = `Can not load: ${codeFile}`;
            }
        }

        modal.classList.add('show');
    });
});

modal.addEventListener('click', (e) => {
    if (e.target === modalcontent) {
        modal.classList.remove('show');
    }
});

closeBtn.addEventListener('click', () => {
    modal.classList.remove('show');
});