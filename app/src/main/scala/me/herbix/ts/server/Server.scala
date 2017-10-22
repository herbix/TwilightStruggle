// Copyright (C) 2017 Chaofan

package me.herbix.ts.server

import java.io.FileWriter
import java.lang.management.ManagementFactory

import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.channel.{ChannelInitializer, ChannelOption, EventLoopGroup}
import me.herbix.ts.netcommon.NetCodec
import me.herbix.ts.util.Config

import scala.collection.mutable

/**
  * Created by Chaofan on 2016/7/3.
  */
object Server {
  val netHandlers = mutable.Map.empty[Int, NetHandlerServer]
  val rooms = mutable.Map.empty[Int, Room]

  def main(args: Array[String]): Unit = {
    if (Config.pidFile != null) {
      savePidFile()
    }

    val bossGroup: EventLoopGroup = new NioEventLoopGroup()
    val workerGroup: EventLoopGroup = new NioEventLoopGroup()

    try {
      val bootstrap = new ServerBootstrap()
      bootstrap.group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childHandler(new ChannelInitializer[SocketChannel] {
          override def initChannel(ch: SocketChannel): Unit = {
            println(s"Receive connection ${ch.remoteAddress()}")
            ch.pipeline().addLast(new NetCodec, new NetHandlerServer)
          }
        })
        .option(ChannelOption.SO_BACKLOG.asInstanceOf[ChannelOption[Any]], 128)
        .childOption(ChannelOption.SO_KEEPALIVE.asInstanceOf[ChannelOption[Any]], true)

      val channelFuture = bootstrap.bind(Config.port).sync()

      channelFuture.channel().closeFuture().sync()
    } finally {
      bossGroup.shutdownGracefully()
      workerGroup.shutdownGracefully()
    }
  }

  var id = 0
  def nextId(): Int = {
    id += 1
    id
  }

  private def savePidFile() {
    try {
      val fw = new FileWriter(Config.pidFile)
      fw.write(getPid)
      fw.close()
    } catch {
      case e: Exception => println("Cannot create pid file '" + Config.pidFile + "'")
    }
  }

  private def getPid: String = {
    ManagementFactory.getRuntimeMXBean.getName.split("@")(0)
  }
}
