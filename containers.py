import asyncio
import aiodocker

async def list_things():
    docker = aiodocker.Docker()
    print('== Images ==')
    for image in (await docker.images.list()):
        tags = str(image['RepoTags']) if image['RepoTags'] else ''
        print(image['Id'], tags)
    print('== Containers ==')
    for container in (await docker.containers.list()):
        print(f" {container._id}")
    await docker.close()

if __name__ == '__main__':
    loop = asyncio.new_event_loop()
    asyncio.set_event_loop(loop)
    loop.run_until_complete(list_things())
    loop.close()
