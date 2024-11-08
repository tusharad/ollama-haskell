sayHello:
		echo "hello"

install-ollama:
	curl -fsSL https://ollama.com/install.sh | sh
	ollama
	ollama pull llama3.2
